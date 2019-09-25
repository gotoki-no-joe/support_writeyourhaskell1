---------- p.093

// LZVal
lzEval() : boolean { return this.r.lzEval(this); }
set(r1: R12N) { this.r = r1; }

// R12N
lzEval(me : LZVal) : boolean;

---------- p.094 List 6.1

class LZVal {
  constructor(private r: R12N) { }
  get(): R12N {
    let r = this.r;
    while (r instanceof RRef) { r = r.ptr.r; }
    return r;
  }
}

type ASSOC = Map<string, LZVal>;

interface R12N { }

class RInt implements R12N {
  constructor(public val: number) { }
}

class RVarRef implements R12N {
  constructor(private env: Env, private name: string) { }
}

class RApp implements R12N {
  constructor(private lvs: LZVal[]) { }
}

class RSource implements R12N {
  constructor(private env: Env, t: AST) { }
}

class RRef implements R12N {
    constructor(public ptr: LZVal) { }
}

---------- p.095 List 6.2

abstract class RClosure implements R12N { }

class RLamClosure extends RClosure {
  constructor(private a: ASSOC, private ps: string[],
              private ext: Env, private e: AST,
              private idx: number) { super(); }
}

class RBinClosure extends RClosure {
  constructor(private args: Value[], private arity: number,
              private name: string,
              private func: (arg: Value[]) => Value) { }
}

class RADTClosure extends RClosure {
  constructor(public name: string,
              public arity: number,
              public args: Value[]) { super(); }
}

-----

// RInt
lzEval(me : LZVal) { return false; }

-----

// RVarRef
lzEval(me: LZVal) {
  me.set(new RRef(this.env.get(this.name)));
  return true;
}

-----

// RRef
lzEval(me: LZVal) { return this.ptr.lzEval(); }

---------- p.096

// RClosure
abstract lzEval(me: LZVal): boolean;

// RADTClosure
lzEval(me: LZVal) { return false; }

-----

// RLamClosure
lzEval(me: LZVal) {
  if (this.idx == this.ps.length) {
    const env = new Env(this.a, this.ext);
    const s = new RSource(env, this.e);
    me.set(s);
    return true;
  }
  return false;
}

-----

// RBinClosure
lzEval(me: LZVal) {
  if (this.arity > 0) { return false; }
  for (const a of this.args) {
    if (a.lzEval()) { return true; }
  }
  const as = this.args.map((a) => a.get());
  const res = this.func(as);
  me.set(res);
  return true;
}

---------- p.097

// RApp
lzEval(me: LZVal) {
  if (this.lvs.length == 1) {
    me.set(new RRef(this.lvs[0]));
    return true;
  }
  const r0 = this.lvs[0].get();
  if (r0 instanceof RClosure) {
    if (r0.saturated()) {
      return this.lvs[0].lzEval();
    }
    const lv0n = new LZVal(r0.apply(this.lvs[1]));
    const lvs1 = this.lvs.slice(1);
    lvs1[0] = lv0n;
    const app2 = new RApp(lvs1);
    me.set(app2);
    return true;
  } else {
    const b = this.lvs[0].lzEval();
    if (!b) { throw "can't reduce to closure"; }
    return b;
} }

-----

// RClosure
abstract saturated(): boolean;
abstract apply(arg: LZVal): RClosure;

// RLamClosure
saturated() { return (this.idx == this.ps.length); }
apply(arg: LZVal): RLamClosure {
  const a1 = new Map(this.a); // clone
  a1.set(this.ps[this.idx], arg);
  return new RLamClosure(a1, this.ps, this.ext, this.e, this.idx+1);
}

// RBinClosure
saturated() { return (this.arity == 0); }
apply(arg: LZVal): RBinClosure {
  const args1 = this.args.slice(); // clone
  args1.push(arg);
  return new RBinClosure(args1, this.arity-1, this.name, this.func);
}

// RADTClosure
saturated() { return (this.arity == 0); }
apply(arg: LZVal): RADTClosure {
  const args1 = this.args.slice(); // clone
  args1.push(arg);
  return new RADTClosure(this.name, this.arity-1, args1);
}

---------- p.098

// RSource
lzEval(me: LZVal) {
  me.set(this.t.toR12N(this.env));
  return true;
}
// AST
abstract toR12N(env: Env): R12N;
// ASTInt
toR12N(env: Env) { return new RInt(this.val); }
// ASTVarRef
toR12N(env: Env) { return new RVarRef(env, this.name); }
// ASTLambda
toR12N(env: Env) {
  return new RLamClosure(new Map(), this.ps, env, this.e, 0); }
// ASTApp
toR12N(env: Env) {
  const lvs = [];
  for (const e of this.es)
    { lvs.push(new LZVal(new RSource(env, e))); }
  return new RApp(lvs);
}
// ASTConst
toR12N(env: Env) { return new RVarRef(env, this.name); }
// ASTLet
toR12N(env: Env) {
  const m = new Map();
  const env1 = new Env(m, env);
  for (const vd of this.vds) {
    m.set((vd.p as PASTVar).name,
          new LZVal(new RSource(env1, vd.t)));
  }
  return new RSource(env1, this.e);
}

---------- p.099

function compile(root: Env, prog: H2Prog): Env {
  const env1 = new Env(new Map(), root);
  for (const d of prog) { d.register(env1); }
  return env1;
}

// DAST
abstract register(env: Env): void;

// DASTVDef
register(env: Env) {
  env.assoc.set((this.p as PASTVar).name,
                new LZVal(new RSource(env, this.t)));
}

// DASTData
register(env: Env) {
  for (const k of this.ks) {
    env.assoc.set(k.name,
      new LZVal(new RADTClosure(k.name, k.args.length, [])));
  }
}

---------- p.100

let rootEnv: Env;
{
  const m = new Map();
  m.set("x", new LZVal(new RInt(999)));
  m.set("add", new LZVal(new RBinClosure([], 2, "add", addFunc)));
  m.set("sub", new LZVal(new RBinClosure([], 2, "sub", subFunc)));
  m.set("True", new LZVal(new RADTClosure("True", 0, [])));
  m.set("False", new LZVal(new RADTClosure("False", 0, [])));
  rootEnv = new Env(m);
}
function addFunc(args: R12N[]) {
  const a = (args[0] as RInt).val;
  const b = (args[0] as RInt).val;
  console.log("add", a, b);
  return new RInt(a + b);
}
function subFunc(args: R12N[]) {
  const a = (args[0] as RInt).val;
  const b = (args[0] as RInt).val;
  console.log("sub", a, b);
  return new RInt(a - b);
}

-----

const prelude = `
data List a = Nil | Cons a (List a);
zero = \\x -> sub x x;`;

-----

let progEnv: Env;

function preload() {
  const res = pProgram.run(new State(prelude));
  if (res instanceof ResultOK) {
    progEnv = compile(rootEnv, res.value);
    console.log("compile OK", progEnv);
  } else {
    console.log("compile error");
} }

function rep(src: string) {
  console.log("Source::", src);
  const st = new State(src);
  const res = pExpr.run(st);
  if (res instanceof ResultOK) {
    let v = new LZVal(new RSource(progEnv, res.value));
    while (v.lzEval()) { }
    console.log("Evaluated::", v);
} }
