---------- p.043

const spaces = new RegExpP(/\s*/y);

-----

new RegExpP(/\d+/y) // many1 digit に相当

-----

const pInt = new Seq<number>(function*() {
  const ds = yield (new RegExpP(/\d+/y));
  yield spaces;
  return parseInt(ds, 10); });

---------- p.044

const pInt = new Seq<number>(function*() {
  const ds = yield pIntDS;
  yield spaces;
  return parseInt(ds, 10);
});
const pIntDS = new RegExpP(/\d+/y);

-----

const pVar = new Seq<string>(function*() {
  const nm = yield pVarNM;
  yield spaces;
  return nm;
});
const pVarNM = new RegExpP(/[a-z][A-Za-z0-9_]*/y);

-----

const pExprVar : Parser<AST> = pVar.apply((v) => new ASTVarRef(v));

---------- p.045

const pExprInt : Parser<AST> = pInt.apply((n) => new ASTInt(n));

-----

const pParen = new Seq<AST>(function*() {
  yield pParenOpen; yield spaces;
  const e = yield pExpr;
  yield pParenClose; yield spaces;
  return e;
});
const pParenOpen  = new StringP("(");
const pParenClose = new StringP(")");

-----

const pLambda = new Seq<AST>(function*() {
  yield pLambdaBS; yield spaces;
  const xs = yield pLambdaVS;
  yield pLambdaAR; yield spaces;
  const e = yield pExpr;
  return new ASTLambda(xs, e);
});
const pLambdaBS = new StringP("\\");
const pLambdaVS = pVar.many(true);
const pLambdaAR = new StringP("->");

-----

const pLet = new Seq<AST>(function*() {
  yield pLetLet; yield spaces;
  const vds = yield pVDefs;
  yield pLetIn; yield spaces;
  const e = yield pExpr;
  return new ASTLet(vds,e);
});
const pLetLet = new StringP("let");
const pLetIn = new StringP("in");

---------- p.046

const pVDefs = new Seq(function*() {
    const x = yield pVar;
    yield pEqual; yield spaces;
    const e = yield pExpr;
    yield pSemicolon; yield spaces;
    return { x : x, e : e };
}).many(true);
const pEqual = new StringP("=");
const pSemicolon = new StringP(";");

-----

const pCExpr = new Select<AST>();

-----

const pExpr = pCExpr.many(true).apply((es) => {
  return (es.length == 1) ? es[0] : new ASTApp(es);
});

---------- p.047

pCExpr.setup([pParen,pLambda,pLet,pExprVar,pExprInt]);

-----

function stEval(src:string) {
  console.log("Source::", src);
  const st = new State(src);
  const res = pExpr.run(st);
  console.log("Result::", res);
  if (res instanceof ResultOK) {
    console.log("Show::", res.value.show());
    const v = res.value.stEval(testEnv);
    console.log("Evaluated::", v);
} }

function test3() {
  stEval("123");
  stEval("x");
  stEval("add x 1");
  stEval("let y = 1; in add x y");
  stEval("let x = 456; in x");
  stEval("(\\ x -> add x x) 3");
  stEval("let s = add 1; in add (s 2) (s 3)");
}
test3();

---------- p.048

class Fail<A> extends Parser<A> {
  run(state : State) { return new ResultFail(state); }
}

-----

const pVar = new Seq(function*() {
  const s = yield pVarNM; yield spaces;
  if (keywords.has(s)) { yield pFail; }
  return s;
});
const keywords = new Set(["let", "in"]);
const pFail = new Fail();

---------- p.049

const pInt2 = new RegExpP(/\d+\s*/y).apply((ds) => parseInt(ds,10));

-----

const pExprInt2 = new RegExpP(/\d+\s*/y).
                  apply((ds) => new ASTInt(parseInt(ds,10)));
