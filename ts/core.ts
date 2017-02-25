import * as fs from "fs";

import { readline } from "./node_readline";

import { Node, MalType, MalSymbol, MalFunction, MalNull, MalList, MalVector, MalBoolean, MalNumber, MalString, MalKeyword, MalHashMap, MalAtom, equals, isSeq } from "./types";
import { readStr } from "./reader";
import { prStr } from "./printer";

export const ns: Map<MalSymbol, MalFunction> = (() => {
    const ns: { [symbol: string]: typeof MalFunction.prototype.func; } = {
        "="(a: MalType, b: MalType): MalBoolean {
            return new MalBoolean(equals(a, b));
        },
        throw(v: MalType): MalType {
            throw v;
        },

        "nil?"(v: MalType) {
            return new MalBoolean(v.type === Node.Null);
        },
        "true?"(v: MalType) {
            return new MalBoolean(v.type === Node.Boolean && v.v);
        },
        "false?"(v: MalType) {
            return new MalBoolean(v.type === Node.Boolean && !v.v);
        },
        "string?"(v: MalType) {
            return new MalBoolean(v.type === Node.String);
        },
        symbol(v: MalType) {
            if (v.type !== Node.String) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return MalSymbol.get(v.v);
        },
        "symbol?"(v: MalType) {
            return new MalBoolean(v.type === Node.Symbol);
        },
        keyword(v: MalType) {
            if (v.type !== Node.String) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return MalKeyword.get(v.v);
        },
        "keyword?"(v: MalType) {
            return new MalBoolean(v.type === Node.Keyword);
        },

        "pr-str"(...args: MalType[]): MalString {
            return new MalString(args.map(v => prStr(v, true)).join(" "));
        },
        "str"(...args: MalType[]): MalString {
            return new MalString(args.map(v => prStr(v, false)).join(""));
        },
        prn(...args: MalType[]): MalNull {
            const str = args.map(v => prStr(v, true)).join(" ");
            console.log(str);
            return MalNull.instance;
        },
        println(...args: MalType[]): MalNull {
            const str = args.map(v => prStr(v, false)).join(" ");
            console.log(str);
            return MalNull.instance;
        },
        "read-string"(v: MalType) {
            if (v.type !== Node.String) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            return readStr(v.v);
        },
        readline(v: MalType) {
            if (v.type !== Node.String) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }

            const ret = readline(v.v);
            if (ret == null) {
                return MalNull.instance;
            }

            return new MalString(ret);
        },
        slurp(v: MalType) {
            if (v.type !== Node.String) {
                throw new Error(`unexpected symbol: ${v.type}, expected: string`);
            }
            const content = fs.readFileSync(v.v, "UTF-8");
            return new MalString(content);
        },

        "<"(a: MalType, b: MalType): MalBoolean {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v < b.v);
        },
        "<="(a: MalType, b: MalType): MalBoolean {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v <= b.v);
        },
        ">"(a: MalType, b: MalType): MalBoolean {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v > b.v);
        },
        ">="(a: MalType, b: MalType): MalBoolean {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalBoolean(a.v >= b.v);
        },
        "+"(a: MalType, b: MalType): MalNumber {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v + b.v);
        },
        "-"(a: MalType, b: MalType): MalNumber {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v - b.v);
        },
        "*"(a: MalType, b: MalType): MalNumber {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v * b.v);
        },
        "/"(a: MalType, b: MalType): MalNumber {
            if (a.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${a.type}, expected: number`);
            }
            if (b.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${b.type}, expected: number`);
            }

            return new MalNumber(a.v / b.v);
        },
        "time-ms"() {
            return new MalNumber(Date.now());
        },

        list(...args: MalType[]): MalList {
            return new MalList(args);
        },
        "list?"(v: MalType): MalBoolean {
            return new MalBoolean(v instanceof MalList);
        },
        vector(...args: MalType[]): MalVector {
            return new MalVector(args);
        },
        "vector?"(v: MalType): MalBoolean {
            return new MalBoolean(v.type === Node.Vector);
        },
        "hash-map"(...args: MalType[]) {
            return new MalHashMap(args);
        },
        "map?"(v: MalType): MalBoolean {
            return new MalBoolean(v.type === Node.HashMap);
        },
        assoc(v: MalType, ...args: MalType[]) {
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            return v.assoc(args);
        },
        dissoc(v: MalType, ...args: MalType[]) {
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            return v.dissoc(args);
        },
        get(v: MalType, key: MalType) {
            if (v.type === Node.Null) {
                return MalNull.instance;
            }
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            if (key.type !== Node.String && key.type !== Node.Keyword) {
                throw new Error(`unexpected symbol: ${key.type}, expected: string or keyword`);
            }

            return v.get(key) || MalNull.instance;
        },
        "contains?"(v: MalType, key: MalType) {
            if (v.type === Node.Null) {
                return MalNull.instance;
            }
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }
            if (key.type !== Node.String && key.type !== Node.Keyword) {
                throw new Error(`unexpected symbol: ${key.type}, expected: string or keyword`);
            }

            return new MalBoolean(v.has(key));
        },
        keys(v: MalType) {
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }

            return new MalList([...v.keys()]);
        },
        vals(v: MalType) {
            if (v.type !== Node.HashMap) {
                throw new Error(`unexpected symbol: ${v.type}, expected: hash-map`);
            }

            return new MalList([...v.vals()]);
        },

        "sequential?"(v: MalType) {
            return new MalBoolean(isSeq(v));
        },
        cons(a: MalType, b: MalType) {
            if (!isSeq(b)) {
                throw new Error(`unexpected symbol: ${b.type}, expected: list or vector`);
            }

            return new MalList([a].concat(b.list));
        },
        concat(...args: MalType[]) {
            const list = args
                .map(arg => {
                    if (!isSeq(arg)) {
                        throw new Error(`unexpected symbol: ${arg.type}, expected: list or vector`);
                    }
                    return arg;
                })
                .reduce((p, c) => p.concat(c.list), [] as MalType[]);

            return new MalList(list);
        },
        nth(list: MalType, idx: MalType) {
            if (!isSeq(list)) {
                throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
            }
            if (idx.type !== Node.Number) {
                throw new Error(`unexpected symbol: ${idx.type}, expected: number`);
            }

            const v = list.list[idx.v];
            if (!v) {
                throw new Error("nth: index out of range");
            }

            return v;
        },
        first(v: MalType) {
            if (v.type === Node.Null) {
                return MalNull.instance;
            }
            if (!isSeq(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: list or vector`);
            }

            return v.list[0] || MalNull.instance;
        },
        rest(v: MalType) {
            if (v.type === Node.Null) {
                return new MalList([]);
            }
            if (!isSeq(v)) {
                throw new Error(`unexpected symbol: ${v.type}, expected: list or vector`);
            }

            return new MalList(v.list.slice(1));
        },
        "empty?"(v: MalType): MalBoolean {
            if (!isSeq(v)) {
                return new MalBoolean(false);
            }
            return new MalBoolean(v.list.length === 0);
        },
        count(v: MalType): MalNumber {
            if (isSeq(v)) {
                return new MalNumber(v.list.length);
            }
            if (v.type === Node.Null) {
                return new MalNumber(0);
            }
            throw new Error(`unexpected symbol: ${v.type}`);
        },
        apply(f: MalType, ...list: MalType[]) {
            if (f.type !== Node.Function) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }

            const tail = list[list.length - 1];
            if (!isSeq(tail)) {
                throw new Error(`unexpected symbol: ${tail.type}, expected: list or vector`);
            }
            const args = list.slice(0, -1).concat(tail.list);
            return f.func(...args);
        },
        map(f: MalType, list: MalType) {
            if (f.type !== Node.Function) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }
            if (!isSeq(list)) {
                throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
            }

            return new MalList(list.list.map(v => f.func(v)));
        },

        conj(list: MalType, ...args: MalType[]) {
            switch (list.type) {
                case Node.List:
                    const newList = new MalList(list.list);
                    args.forEach(arg => newList.list.unshift(arg));
                    return newList;
                case Node.Vector:
                    return new MalVector([...list.list, ...args]);
            }

            throw new Error(`unexpected symbol: ${list.type}, expected: list or vector`);
        },
        seq(v: MalType) {
            if (v.type === Node.List) {
                if (v.list.length === 0) {
                    return MalNull.instance;
                }
                return v;
            }
            if (v.type === Node.Vector) {
                if (v.list.length === 0) {
                    return MalNull.instance;
                }
                return new MalList(v.list);
            }
            if (v.type === Node.String) {
                if (v.v.length === 0) {
                    return MalNull.instance;
                }
                return new MalList(v.v.split("").map(s => new MalString(s)));
            }
            if (v.type === Node.Null) {
                return MalNull.instance;
            }

            throw new Error(`unexpected symbol: ${v.type}, expected: list or vector or string`);
        },

        meta(v: MalType) {
            return v.meta || MalNull.instance;
        },
        "with-meta"(v: MalType, m: MalType) {
            return v.withMeta(m);
        },
        atom(v: MalType): MalAtom {
            return new MalAtom(v);
        },
        "atom?"(v: MalType): MalBoolean {
            return new MalBoolean(v.type === Node.Atom);
        },
        deref(v: MalType): MalType {
            if (v.type !== Node.Atom) {
                throw new Error(`unexpected symbol: ${v.type}, expected: atom`);
            }
            return v.v;
        },
        "reset!"(atom: MalType, v: MalType): MalType {
            if (atom.type !== Node.Atom) {
                throw new Error(`unexpected symbol: ${atom.type}, expected: atom`);
            }
            atom.v = v;
            return v;
        },
        "swap!"(atom: MalType, f: MalType, ...args: MalType[]): MalType {
            if (atom.type !== Node.Atom) {
                throw new Error(`unexpected symbol: ${atom.type}, expected: atom`);
            }
            if (f.type !== Node.Function) {
                throw new Error(`unexpected symbol: ${f.type}, expected: function`);
            }
            atom.v = f.func(...[atom.v].concat(args));
            return atom.v;
        },
    };

    const map = new Map<MalSymbol, MalFunction>();
    Object.keys(ns).forEach(key => map.set(MalSymbol.get(key), MalFunction.fromBootstrap(ns[key])));
    return map;
})();
