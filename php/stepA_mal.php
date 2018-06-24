<?php

require_once 'readline.php';
require_once 'types.php';
require_once 'reader.php';
require_once 'printer.php';
require_once 'interop.php';
require_once 'env.php';
require_once 'core.php';

// read
function READ($str) {
    return read_str($str);
}

// eval
function is_pair($x) {
    return _sequential_Q($x) and count($x) > 0;
}

function quasiquote($ast) {
    if (!is_pair($ast)) {
        return _list(_symbol("quote"), $ast);
    } elseif (_symbol_Q($ast[0]) && $ast[0]->value === 'unquote') {
        return $ast[1];
    } elseif (is_pair($ast[0]) && _symbol_Q($ast[0][0]) &&
              $ast[0][0]->value === 'splice-unquote') {
        return _list(_symbol("concat"), $ast[0][1],
                     quasiquote($ast->slice(1)));
    } else {
        return _list(_symbol("cons"), quasiquote($ast[0]),
                     quasiquote($ast->slice(1)));
    }
}

function is_macro_call($ast, $env) {
    return is_pair($ast) &&
           _symbol_Q($ast[0]) &&
           $env->find($ast[0]) &&
           $env->get($ast[0])->ismacro;
}

function macroexpand($ast, $env) {
    while (is_macro_call($ast, $env)) {
        $mac = $env->get($ast[0]);
        $args = array_slice($ast->getArrayCopy(),1);
        $ast = $mac->apply($args);
    }
    return $ast;
}

function eval_ast($ast, $env) {
    if (_symbol_Q($ast)) {
        return $env->get($ast);
    } elseif (_sequential_Q($ast)) {
        if (_list_Q($ast)) {
            $el = _list();
        } else {
            $el = _vector();
        }
        foreach ($ast as $a) { $el[] = MAL_EVAL($a, $env); }
        return $el;
    } elseif (_hash_map_Q($ast)) {
        $new_hm = _hash_map();
        foreach (array_keys($ast->getArrayCopy()) as $key) {
            $new_hm[$key] = MAL_EVAL($ast[$key], $env);
        }
        return $new_hm;
    } else {
        return $ast;
    }
}

function MAL_EVAL($ast, $env) {
    $_SUPERGLOBALS = ["_SERVER", "_GET", "_POST", "_FILES", "_REQUEST", "_SESSION", "_ENV", "_COOKIE"];
    while (true) {

    #echo "MAL_EVAL: " . _pr_str($ast) . "\n";
    if (!_list_Q($ast)) {
        return eval_ast($ast, $env);
    }

    // apply list
    $ast = macroexpand($ast, $env);
    if (!_list_Q($ast)) {
        return eval_ast($ast, $env);
    }
    if ($ast->count() === 0) {
        return $ast;
    }

    $a0 = $ast[0];
    $a0v = (_symbol_Q($a0) ? $a0->value : $a0);
    switch ($a0v) {
    case "def!":
        $res = MAL_EVAL($ast[2], $env);
        return $env->set($ast[1], $res);
    case "let*":
        $a1 = $ast[1];
        $let_env = new Env($env);
        for ($i=0; $i < count($a1); $i+=2) {
            $let_env->set($a1[$i], MAL_EVAL($a1[$i+1], $let_env));
        }
        $ast = $ast[2];
        $env = $let_env;
        break; // Continue loop (TCO)
    case "quote":
        return $ast[1];
    case "quasiquote":
        $ast = quasiquote($ast[1]);
        break; // Continue loop (TCO)
    case "defmacro!":
        $func = MAL_EVAL($ast[2], $env);
        $func->ismacro = true;
        return $env->set($ast[1], $func);
    case "macroexpand":
        return macroexpand($ast[1], $env);
    case "php*":
        $res = eval($ast[1]);
        return _to_mal($res);
    case "try*":
        $a1 = $ast[1];
        $a2 = $ast[2];
        if ($a2[0]->value === "catch*") {
            try {
                return MAL_EVAL($a1, $env);
            } catch (Error $e) {
                $catch_env = new Env($env, array($a2[1]),
                                            array($e->obj));
                return MAL_EVAL($a2[2], $catch_env);
            } catch (Exception $e) {
                $catch_env = new Env($env, array($a2[1]),
                                            array($e->getMessage()));
                return MAL_EVAL($a2[2], $catch_env);
            }
        } else {
            return MAL_EVAL($a1, $env);
        }
    case "do":
        eval_ast($ast->slice(1, -1), $env);
        $ast = $ast[count($ast)-1];
        break; // Continue loop (TCO)
    case "if":
        $cond = MAL_EVAL($ast[1], $env);
        if ($cond === NULL || $cond === false) {
            if (count($ast) === 4) { $ast = $ast[3]; }
            else                   { $ast = NULL; }
        } else {
            $ast = $ast[2];
        }
        break; // Continue loop (TCO)
    case "fn*":
        return _function('MAL_EVAL', 'native',
                         $ast[2], $env, $ast[1]);
    case "$":
        $var = MAL_EVAL($ast[1], $env);
        if (_symbol_Q($var)) {
          $varname = $var->value;
        } elseif (gettype($var) === "string") {
          $varname = $var;
        } else {
          throw new Exception("$ arg unknown type: " . gettype($var));
        }
        if (in_array($varname, $_SUPERGLOBALS)) {
            $val = $GLOBALS[$varname];
        } else {
            $val = ${$varname};
        }
        return _to_mal($val);
    case "!":
        $fn = $ast[1]->value;
        $el = eval_ast($ast->slice(2), $env);
        $args = _to_php($el);
        $res = call_user_func_array($fn, $args);
        return _to_mal($res);
    default:
        $el = eval_ast($ast, $env);
        $f = $el[0];
        $args = array_slice($el->getArrayCopy(), 1);
        if ($f->type === 'native') {
            $ast = $f->ast;
            $env = $f->gen_env($args);
            // Continue loop (TCO)
        } else {
            return $f->apply($args);
        }
    }

    }
}

// print
function MAL_PRINT($exp) {
    return _pr_str($exp, True);
}

// repl
$repl_env = new Env(NULL);
function rep($str) {
    global $repl_env;
    return MAL_PRINT(MAL_EVAL(READ($str), $repl_env));
}

// core.php: defined using PHP
foreach ($core_ns as $k=>$v) {
    $repl_env->set(_symbol($k), _function($v));
}
$repl_env->set(_symbol('eval'), _function(function($ast) {
    global $repl_env; return MAL_EVAL($ast, $repl_env);
}));
$_argv = _list();
if (isset($argv)) {
  for ($i=2; $i < count($argv); $i++) {
      $_argv->append($argv[$i]);
  }
}
$repl_env->set(_symbol('*ARGV*'), $_argv);

// core.mal: defined using the language itself
rep("(def! *host-language* \"php\")");
rep("(def! not (fn* (a) (if a false true)))");
rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))");
rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
rep("(def! *gensym-counter* (atom 0))");
rep("(def! gensym (fn* [] (symbol (str \"G__\" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))");
rep("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) (let* (condvar (gensym)) `(let* (~condvar ~(first xs)) (if ~condvar ~condvar (or ~@(rest xs)))))))))");

// run mal file
if (count($argv) > 1) {
    rep('(load-file "' . $argv[1] . '")');
    exit(0);
}

// repl loop
rep("(println (str \"Mal [\" *host-language* \"]\"))");
do {
    try {
        $line = mal_readline("user> ");
        if ($line === NULL) { break; }
        if ($line !== "") {
            print(rep($line) . "\n");
        }
    } catch (BlankException $e) {
        continue;
    } catch (Exception $e) {
        echo "Error: " . $e->getMessage() . "\n";
        echo $e->getTraceAsString() . "\n";
    }
} while (true);

?>
