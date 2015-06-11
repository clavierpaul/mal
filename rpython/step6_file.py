import sys, traceback
import mal_readline
import mal_types as types
from mal_types import (MalSym, MalInt, MalStr,
                       nil, true, false, _symbol, _keywordu,
                       MalList, _list, MalVector, MalHashMap, MalFunc)
import reader, printer
from env import Env
import core

# read
def READ(str):
    return reader.read_str(str)

# eval
def eval_ast(ast, env):
    if types._symbol_Q(ast):
        assert isinstance(ast, MalSym)
        return env.get(ast)
    elif types._list_Q(ast):
        res = []
        for a in ast.values:
            res.append(EVAL(a, env))
        return MalList(res)
    elif types._vector_Q(ast):
        res = []
        for a in ast.values:
            res.append(EVAL(a, env))
        return MalVector(res)
    elif types._hash_map_Q(ast):
        new_dct = {}
        for k in ast.dct.keys():
            new_dct[k] = EVAL(ast.dct[k], env)
        return MalHashMap(new_dct)
    else:
        return ast  # primitive value, return unchanged

def EVAL(ast, env):
    while True:
        #print("EVAL %s" % printer._pr_str(ast))
        if not types._list_Q(ast):
            return eval_ast(ast, env)

        # apply list
        if len(ast) == 0: return ast
        a0 = ast[0]
        if isinstance(a0, MalSym):
            a0sym = a0.value
        else:
            a0sym = u"__<*fn*>__"

        if u"def!" == a0sym:
            a1, a2 = ast[1], ast[2]
            res = EVAL(a2, env)
            return env.set(a1, res)
        elif u"let*" == a0sym:
            a1, a2 = ast[1], ast[2]
            let_env = Env(env)
            for i in range(0, len(a1), 2):
                let_env.set(a1[i], EVAL(a1[i+1], let_env))
            ast = a2
            env = let_env # Continue loop (TCO)
        elif u"do" == a0sym:
            if len(ast) == 0:
                return nil
            elif len(ast) > 1:
                eval_ast(ast.slice2(1, len(ast)-1), env)
            ast = ast[-1] # Continue loop (TCO)
        elif u"if" == a0sym:
            a1, a2 = ast[1], ast[2]
            cond = EVAL(a1, env)
            if cond is nil or cond is false:
                if len(ast) > 3: ast = ast[3] # Continue loop (TCO)
                else:            return nil
            else:
                ast = a2 # Continue loop (TCO)
        elif u"fn*" == a0sym:
            a1, a2 = ast[1], ast[2]
            return MalFunc(None, a2, env, a1, EVAL)
        else:
            el = eval_ast(ast, env)
            f = el.values[0]
            if isinstance(f, MalFunc):
                if f.ast:
                    ast = f.ast
                    env = f.gen_env(el.rest()) # Continue loop (TCO) 
                else:
                    return f.apply(el.rest())
            else:
                raise Exception("%s is not callable" % f)

# print
def PRINT(exp):
    return printer._pr_str(exp)

# repl
class MalEval(MalFunc):
    def apply(self, args):
        return self.EvalFunc(args[0], self.env)

def entry_point(argv):
    repl_env = Env()
    def REP(str, env):
        return PRINT(EVAL(READ(str), env))

    # core.py: defined using python
    for k, v in core.ns.items():
        repl_env.set(_symbol(unicode(k)), MalFunc(v))
    repl_env.set(types._symbol(u'eval'),
                 MalEval(None, env=repl_env, EvalFunc=EVAL))
    mal_args = []
    if len(argv) >= 3:
        for a in argv[2:]: mal_args.append(MalStr(unicode(a)))
    repl_env.set(_symbol(u'*ARGV*'), MalList(mal_args))

    # core.mal: defined using the language itself
    REP("(def! not (fn* (a) (if a false true)))", repl_env)
    REP("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))", repl_env)

    if len(argv) >= 2:
        REP('(load-file "' + argv[1] + '")', repl_env)
        return 0

    while True:
        try:
            line = mal_readline.readline("user> ")
            if line == "": continue
            print(REP(line, repl_env))
        except EOFError as e:
            break
        except reader.Blank:
            continue
        except types.MalException as e:
            print(u"Error: %s" % printer._pr_str(e.object, False))
        except Exception as e:
            print("Error: %s" % e)
            #print("".join(traceback.format_exception(*sys.exc_info())))
    return 0

# _____ Define and setup target ___
def target(*args):
    return entry_point

# Just run entry_point if not RPython compilation
import sys
if not sys.argv[0].endswith('rpython'):
    entry_point(sys.argv)
