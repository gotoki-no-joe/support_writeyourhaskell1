---------- p.064

class ASTConst extends AST { ... } // ASTVarRefと同内容

class ASTCase extends AST {
  constructor(private e:AST, private alts:{p:PAST, b:AST}[])
    { super(); }
  show() : string { } // 略
}

-----

// PAST
abstract match(m : ASSOC, v: Value) : ASSOC;

// PASTWildcard_
match(m : ASSOC, v : Value) : ASSOC { return m; }

---------- p.065 List 5.8

abstract class TAST {
  abstract show() : string;
  showk() : string { return this.show(); }
}

class TASTName extends TAST {
  constructor(private name : string) { super(); }
  show() : string { return this.name; }
}

class TASTVar extends TAST {
  constructor(private name : string) { super(); }
  show() : string { return this.name; }
}

class TASTApp extends TAST {
  constructor(private ts : TAST[]) { super(); }
  show() : string {
    return this.ts.map((t) => t.showk()).join(" ");
  }
  showk() : string { return "(" + this.show() + ")"; }
}

-----

// PASTInt
match(m : ASSOC, v : Value) : ASSOC {
  if (v instanceof ValInt) {
    return (v.val == this.val) ? m : null;
  }
  throw "Type mismatch on pattern matching";
}

---------- p.066 List 5.9

abstract class PAST {
  abstract show() : string;
  showk() : string { return this.show(); }
}

class PASTInt extends PAST {
  constructor(private val : number) { super(); }
  show() : string { return this.val.toString(); }
}

class PASTWildcard_ extends PAST {
  show() : string { return "_"; }
}
const PASTWildcard = new PASTWildcard_();

class PASTVar extends PAST {
  constructor(private name : string) { super(); }
  show() : string { return this.name; }
}

class PASTConst extends PAST {
  constructor(private con : string, private ts : PAST[])
  { super(); }
  show() : string {
    let r : string = this.con;
    for (const t of this.ts) { r += " " + t.showk(); }
    return r;
  }
  showk() : string {
    if (this.ts.length == 0) { return this.con; }
    return "(" + this.show() + ")";
  }
}

---------- p.067 List 5.10

type H2Prog = DAST[];

abstract class DAST { }

class DASTVDef extends DAST {
  constructor(public p : PAST, public t : AST) { super(); }
}

class DASTData extends DAST {
  constructor(public tname : string,
              public args : string[],
              public ks : ConstDecl[]) { super(); }
}

type ConstDecl = { name : string, args : TAST[] };

----- List 5.11

class ValADTClosure implements Value {
  constructor(public name:string,
              public arity:number,
              public args:Value[]) { }
  show() : string // 略
  apply(arg : Value) : Value {
    if (this.arity == 0) { throw("too many arg to " + this.name); }
    const args1 = this.args.slice();
    args1.push(arg);
    return new ValADTClosure(this.name, this.arity-1, args1);
  }
}

---------- p.068

// PASTVar
match(m : ASSOC, v : Value) : ASSOC {
  m.set(this.name, v);
  return m;
}

-----

// PASTConst
match(m : ASSOC, v : Value) : ASSOC {
  if (v instanceof ValADTClosure) {
    if (v.name !== this.con) { return null; }
    if (v.arity > 0) { throw "Type error"; }
    if (this.ts.length !== v.args.length) { throw "Type error"; }
    let i = 0;
    for (const t of this.ts) {
      m = t.match(m, v.args[i++]);
      if (! m) { return m; } // failed
    }
    return m;
  }
  throw "Type mismatch on pattern matching";
}

-----

// ASTCase
stEval(env : Env) : Value {
  const v = this.e.stEval(env);
  for (const alt of this.alts) {
    const assoc = alt.p.match(new Map(), v);
    if (assoc) {
      const env1 = new Env(assoc, env);
      const res = alt.b.stEval(env1);
      return res;
    }
  }
  throw("Case Patterns not Exhaustive");
}

---------- p.069

// DAST
abstract register(env : Env) : boolean;

// DASTVdef
register(env : Env) {
  const v = this.t.stEval(env);
  const m = this.p.match(env.assoc,v);
  return (m !== null);
}

-----

// ASTLet
stEval(env : Env) : Value {
  const env1 = new Env(new Map(), env);
  for (const vd of this.vds) {
    if (! vd.register(env1)) {
      throw "Irrefutable Pattern match failed.";
  } }
  return this.e.stEval(env1);
}

-----

// DASTData
register(env : Env) {
  const m = env.assoc;
  for (const k of this.ks) {
    const v = new ValADTClosure(k.name, k.args.length, []);
    m.set(k.name, v);
  }
  return true;
}

---------- p.070

function compile(root : Env, prog : H2Prog) : Env {
  const env1 = new Env(new Map(), root);
  for (const d of prog) {
    const r = d.register(env1);
    if (! r) { throw "Compile Error"; }
  }
  return env1;
}

-----

function cParen<A>(p : Parser<A>) : Parser<A> {
  return new Seq<A>(function*() {
    yield pParenOpen; yield spaces;
    const e = yield p;
    yield pParenClose; yield spaces;
    return e;
  });
}

const pParen = cParen(pExpr);

---------- p.071

// PASTConst
append(ts1 : PAST[]) { this.ts.push(...ts1); }

const pPattern = new Seq(function* () {
  const p1 = yield pCPattern;
  if (p1 instanceof PASTConst) {
    const ps = yield pPatterns;
    if (ps.length > 0) {
      p1.append(ps);
  } }
  return p1;
});
const pPatterns = pPattern.many(false);

const pCPattern = new Select();

const pPInt = pInt.apply((n) => new PASTInt(n));

const pPWildcard = new Seq(function* () {
  yield pkwUS; yield spaces;
  return PASTWildcard;
});
const pkwUS = new StringP("_");

const pPVar = pVar.apply((x) => new PASTVar(x));

const pPConst = new Seq(function* () {
  const n = yield pType;
  return new PASTConst(n, []);
});

pCPattern.setup([pPInt,pPWildcard,pPVar,pPConst,cParen(pPattern)]);

-----

const pType = new Seq(function*() {
  const s = yield pVarTY; yield spaces;
  return s;
});
const pVarTY = new RegExpP(/[A-Z][A-Za-z0-9_]*/y);

const pCType = new Select<TAST>();

const pTYPE = pCType.many(true).apply((ts) => {
  return (ts.length == 1) ? ts[0] : new TASTApp(ts); });

pCType.setup(
  [pVar.apply((x) => new TASTVar(x)),
   pType.apply((x) => new TASTName(x)),
   cParen(pTYPE)]);

---------- p.072

const pCase = new Seq<AST>(function*() {
  yield pkwCase; yield spaces;
  const e = yield pExpr;
  yield pkwOf; yield spaces;
  yield pkwBO; yield spaces;
  const alts = yield pCaseAlts;
  yield pkwBC; yield spaces;
  return new ASTCase(e, alts);
});
const pkwCase = new StringP("case");
const pkwOf = new StringP("of");
const pkwBO = new StringP("{");
const pkwBC = new StringP("}");
const pCaseAlts = new Seq(function*() {
  const p = yield pPattern;
  yield pLambdaAR; yield spaces;
  const e = yield pExpr;
  yield pSemicolon; yield spaces;
  return {p : p, b : e};
}).many(true);

---------- p.073

const pDASTVdef = new Seq(function* () {
  const p = yield pPattern;
  yield pEqual; yield spaces;
  const e = yield pExpr;
  yield pSemicolon; yield spaces;
  return new DASTVDef(p, e);
});

const pDASTVDefs = pDASTVdef.many(true);

-----

const pDASTData = new Seq(function*() {
  yield pkwData; yield spaces;
  const tn = yield pType;
  const ta = yield pVars0;
  yield pEqual; yield spaces;
  const rhs = [];
  let c : string;
  do {
    const name = yield pType;
    const args = yield pCTypes0;
    rhs.push({name, args});
    c = yield pDataRhsC; yieldspaces;
  } while (c == "|");
  return new DASTData(tn,ta,rhs);
});
const pkwData = new StringP("data");
const pVars0 = pVar.many(false);
const pCTypes0 = pCType.many(false);
const pDataRhsC = new RegExpP(/[|;]/y);

---------- p.074

const pProgram = new Seq<H2Prog>(function*() {
  yield(spaces);
  const prog = yield(pLines);
  yield(EOF);
  return prog;
});
const pLines = new Select().setup([pDASTData,pDASTVdef]).many(false);

-----

class EOF_ extends Parser<void> {
  run(state : State) {
    return (state.position == state.source.length) ?
           new ResultOK(state, undefined) : new ResultFail(state);
} }
const EOF = new EOF_();

-----

let rootEnv : Env;
{
  const m = new Map();
  m.set("x", new ValInt(999));
  m.set("add", new ValBinClosure([], 2, "add", (args:Value[]) =>
    new ValInt((args[0] as ValInt).val + (args[1] as ValInt).val)));
  m.set("sub", new ValBinClosure([], 2, "sub", (args:Value[]) =>
    new ValInt((args[0] as ValInt).val - (args[1] as ValInt).val)));
  m.set("signum",new ValBinClosure([],1,"signum",(args:Value[]) =>
    new ValInt(Math.sign((args[0] as ValInt).val))));
  m.set("True", new ValADTClosure("True", 0, []));
  m.set("False", new ValADTClosure("False", 0, []));
  rootEnv = new Env(m);
}

---------- p.075

const prelude = `
mult = \\x y -> if0 y then 0 else add x (mult x (sub y 1));
div = \\x y -> let p = sub x y; q = add 1 (signum p);
               in if0 q then 0 else add 1 (div p y);

data List a = Nil | Cons a (List a);
head = \\l -> case l of { Cons x _ -> x; };
null = \\l -> case l of { Nil -> True; _ -> False; };
`;

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
    const v = res.value.stEval(progEnv);
    console.log("Evaluated::", v);
} }

function test4() {
  rep("mult 9 9");
  rep("div 65 8");
  rep(
`let len = \\l -> case l of { Nil -> 0; Cons _ l -> add 1 (len l); };
in len(Cons 1(Cons 2(Cons 3 Nil)))`);
  rep(
`let sum = \\l -> case l of { Nil -> 0; Cons x l -> add x (sum l); };
in sum(Cons 1(Cons 2(Cons 3 Nil)))`);
}

preload();
test4();
