---------- p.114

interface PatternVisitor<A> {
  visitInt(a: PASTInt): A;
  visitWildCard(w: PASTWildcard_): A;
  visitVar(v: PASTVar): A;
  visitConst(c: PASTConst): A;
}

// PAST
abstract accept<A>(v: PatternVisitor<A>): A;
// PASTInt
accept<A>(v: PatternVisitor<A>): A { return v.visitInt(this); }
// PASTWildcard_
accept<A>(v: PatternVisitor<A>): A { return v.visitWildCard(this); }
// PASTVar
accept<A>(v: PatternVisitor<A>): A { return v.visitVar(this); }
// PASTConst
accept<A>(v: PatternVisitor<A>): A { return v.visitConst(this); }

---------- p.115 List 7.7

class Matcher implements PatternVisitor<boolean> {
  public assoc: ASSOC;
  private ps: PAST[];
  private lvs: LZVal[];
  public fail: boolean;
  constructor(p: PAST, lv: LZVal) {
    this.assoc = new Map();
    this.ps = [p];
    this.lvs = [lv];
    this.fail = false;
  }
  done() { return (this.ps.length == 0); }
  makeEnv(p: Env) { return new Env(this.assoc, p); }
  lzEval(): boolean {
    const p = this.ps[this.ps.length - 1];
    return p.accept(this);
  }
  // ここにvisitorメソッドが入る
}

-----

visitWildCard(w: PASTWildcard_) {
  this.ps.pop();
  this.lvs.pop();
  return true;
}

-----

visitVar(v: PASTVar) {
  this.ps.pop();
  this.assoc.set(v.name, this.lvs.pop());
  return true;
}

---------- p.116

visitInt(i: PASTInt) {
  const lv = this.lvs[this.lvs.length - 1];
  const r = lv.get();
  if (r instanceof RInt) {
    if (i.val == r.val) {
      this.ps.pop();
      this.lvs.pop();
      return true;
    }
    this.fail = true;
    return true;
  }
  if (lv.lzEval()) { return true; } else { throw "type error"; }
}

-----

visitConst(c: PASTConst) {
  const lv = this.lvs[this.lvs.length - 1];
  const r = lv.get();
  if (r instanceof RADTClosure) {
    if (! r.saturated()) { throw "type error"; }
    if (c.con != r.name) {
      this.fail = true;
      return true;
    }
    if (c.ts.length != r.args.length) { throw "type error"; }
    this.ps.pop();
    this.lvs.pop();
    for (let i = c.ts.length - 1; i >= 0; i--) {
      this.ps.push(c.ts[i]);
      this.lvs.push(r.args[i]);
    }
    return true;
  }
  if (lv.lzEval()) { return true; } else { throw "type error"; }
} }

---------- p.117

class RCase implements R12N {
  private mr: Matcher;
  private idx: number;
  constructor(private alts: { p: PAST, b: AST }[],
              private env: Env, private lv: LZVal) {
    this.idx = -1;
    this.reload();
  }
  reload() {
    this.idx++;
    if (this.idx == this.alts.length)
      { throw "case pattern not exhaustive"; }
    this.mr = new Matcher(this.alts[this.idx].p, this.lv);
  }
  lzEval(me: LZVal) {
    if (this.mr.fail) {        // マッチ失敗
      this.reload();
      return true;
    }
    if (this.mr.done()) {      // マッチ成功
      me.set(new RSource(this.mr.makeEnv(this.env),
                         this.alts[this.idx].b));
      return true;
    }
    return this.mr.lzEval();   // マッチ進行中
} }

-----

// ASTCase
toR12N(env: Env) {
  return new RCase(this.alts, env,
                   new LZVal(new RSource(env,this.e)));
}

---------- p.118

class RResolving implements R12N {
  constructor(private env: Env, private mr: Matcher) { }
  lzEval(me: LZVal) {
    if (this.mr.fail) { throw "irrefutable pattern"; }
    if (this.mr.done()) {
      for (const [k,v] of this.mr.assoc.entries()) {
        const x = this.env.get(k);
        x.set(new RRef(v));
      }
      return true;
    }
    return this.mr.lzEval();
} }

-----

// DASTVDef
register(env: Env) {
  const rhs = new LZVal(new RSource(env, this.t));
  const mr = new Matcher(this.p, rhs);
  const r = new LZVal(new RResolving(env, mr));
  const vi = new vcollector();
  this.p.accept(vi);
  for (const v of vi.vs) { env.assoc.set(v, r); }
}

-----

class vcollector implements PatternVisitor<void> {
  public vs:string[];
  constructor() { this.vs = []; }
  visitInt(i: PASTInt) { }
  visitWildCard(w: PASTWildcard_) { }
  visitVar(v: PASTVar) { this.vs.push(v.name); }
  visitConst(c: PASTConst) {
    for (const p of c.ts) { p.accept(this); }
} }

---------- p.119

const prelude = `
t = \\x y z ->
   case signum (sub y x) of {
       1 -> y;
       0 -> y;
       _ -> t (t (sub x 1) y z) (t (sub y 1) z x) (t (sub z 1) x y);
   };
`;

function tarai(x,y,z) {
  if (x <= y) { return y; }
  return tarai(tarai(x-1,y,z), tarai(y-1,z,x), tarai(z-1,x,y));
}
