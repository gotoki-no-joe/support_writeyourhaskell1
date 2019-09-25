---------- p.023 List 2.1

abstract class AST { }

class ASTInt extends AST {
    constructor(private val : number) { super(); }
}

class ASTVarRef extends AST {
    constructor(private name : string) { super(); }
}

class ASTLambda extends AST {
    constructor(private ps : string[], private e : AST) { super(); }
}

class ASTLet extends AST {
    constructor(private vds : ASTVDef[],
                private e : AST) { super(); }
}

class ASTApp extends AST {
    constructor(private es : AST[]) { super(); }
}

class ASTVDef {
    constructor(public x : string, public e : AST) { }
}

---------- p.024 List 2.2

const varx = new ASTVarRef("x");
const fadd = new ASTVarRef("add");
const vars = new ASTVarRef("s");
const example1 = new ASTInt(123);
const example2 = varx;
const example3 = new ASTApp([fadd, varx, new ASTInt(1)]);
const example4 = new ASTLet([{x:"y",e:new ASTInt(1)}],
  new ASTApp([fadd, varx, new ASTVarRef("y")]));
const example5 = new ASTLet([{x:"x",e:new ASTInt(456)}],varx);
const example6 = new ASTApp([new ASTLambda(["x"],
  new ASTApp([fadd, varx, varx])),new ASTInt(3)]);
const example7 = new ASTLet(
  [{x:"s",e:new ASTApp([fadd, new ASTInt(1)])}],
  new ASTApp([fadd, new ASTApp([vars, new ASTInt(2)]),
                    new ASTApp([vars, new ASTInt(3)])]));

---------- p.025 List 2.3

// AST
abstract show() : string;
showk() : string { return this.show(); }

// ASTInt
show() : string { return this.val.toString(); }

// ASTVarRef
show() : string { return this.name; }

// ASTLambda
show() : string { return "\\" + this.ps.join(" ") +
                         " -> " + this.e.show(); }
showk() : string { return "(" + this.show() + ")"; }
// ASTLet
show() : string {
  let r : string = "let ";
  for (const xe of this.vds) {
    r += xe.x + " = " + xe.e.show() + "; ";
  };
  r += "in " + this.e.show();
  return r;
}
showk() : string { return "(" + this.show() + ")"; }

// ASTApp
show() : string { return this.es.map((e) => e.showk()).join(" "); }
showk() : string { return "(" + this.show() + ")"; }

---------- p.026 List 2.4

type ASSOC = Map<string,Value>;

class Env {
  constructor(public assoc : ASSOC, private parent? : Env) { }
  get(name : string) {
    const v = this.assoc.get(name);
    if (v) { return v; }
    if (this.parent) { return this.parent.get(name); }
    throw name + "not defined";
} }

-----

// ASTLet
stEval(env : Env) : Value {
  const m = new Map();
  const env1 = new Env(m, env);
  for (const xe of this.vds) {
    m.set(xe.x, xe.e.stEval(env1));
  }
  return this.e.stEval(env1);
}

---------- p.027 List 2.5

interface Value {
  show() : string;
}

class ValInt implements Value {
  constructor(public val : number) { }
  show() : string { return this.val.toString(); }
}

class ValLamClosure implements Value {
  constructor(private a : ASSOC, private ps : string[],
              private ext : Env, private e : AST,
              private idx : number) { }
  show() : string { return "[LambdaClosure]"; }
}

class ValBinClosure implements Value {
  constructor(private args : Value[], private arity : number,
              private name : string,
              private func : (arg : Value[]) => Value) { }
  show() : string { return this.name; }
}

-----

let testEnv : Env;
{ const m = new Map();
  m.set("x", new ValInt(999));
  m.set("add", new ValBinClosure([], 2, "add",
    (args:Value[]) => new ValInt((args[0] as ValInt).val +
                                 (args[1] as ValInt).val)));
  testEnv = new Env(m); }

---------- p.028 List 2.6

// Value
apply(arg : Value) : Value;

// ValInt
apply(arg : Value) : Value { throw "cannot apply to ValInt"; }

// ValLamClosure
apply(arg : Value) : Value {
  const a1 = new Map(this.a); // clone
  let idx = this.idx;
  const p = this.ps[idx++];
  a1.set(p, arg);
  if (idx < this.ps.length) {
    return new ValLamClosure(a1,this.ps,this.ext,this.e,idx);
  } else {
    return this.e.stEval(new Env(a1, this.ext));
  }
}

// ValBinClosure
apply(arg : Value) : Value {
  const args1 = this.args.slice(); // clone
  args1.push(arg);
  const arity1 = this.arity - 1;
  if (arity1 == 0) {
    return this.func(args1);
  } else {
    return new ValBinClosure(args1, arity1, this.name, this.func);
  }
}

---------- p.029 List 2.7

// AST
abstract stEval(env : Env) : Value;

// ASTInt
stEval(env : Env) : Value { return new ValInt(this.val); }

// ASTVarRef
stEval(env : Env) : Value { return env.get(this.name); }

// ASTLambda
stEval(env : Env) : Value {
  return new ValLamClosure(new Map(), this.ps, env, this.e, 0);
}

// ASTLet
stEval(env : Env) : Value {
  const m = new Map();
  for (const xe of this.vds) {
    m.set(xe.x, xe.e.stEval(env));
  }
  return this.e.stEval(new Env(m, env));
}

// ASTApp
stEval(env : Env) : Value {
  let i = 0;
  let v0 = this.es[i++].stEval(env);
  while (i < this.es.length) {
    const v1 = this.es[i++].stEval(env);
    v0 = v0.apply(v1);
  }
  return v0;
}
