---------- p.032 List 3.1

class State {
  constructor(public source: string,
              public position: number = 0) { }
}

abstract class Result<A> {
  state : State;
}

class ResultOK<A> extends Result<A> {
  constructor(public state: State, public value: A) { super(); }
}

class ResultFail<A> extends Result<A> {
  constructor(public state: State) { super(); }
}

abstract class Parser<A> {
  abstract run(state : State) : Result<A>;
}

---------- p.033 List 3.2

class StringP extends Parser<string> {
  constructor(private text : string) { super(); }
  run(state : State) : Result<string> {
    const slice = state.source.slice(state.position,
                    state.position + this.text.length);
    if (this.text == slice) {
      return new ResultOK<string>(state.seek(this.text.length),
                                  this.text);
    } else {
      return new ResultFail<string>(state);
} } }

-----

seek(ofs : number) {
  return new State(this.source, this.position + ofs); }

---------- p.034 List 3.3

class RegExpP extends Parser<string> {
  constructor(private pattern : RegExp) {
    super();
    if (! pattern.sticky) {
      throw "Pattern not sticky";
  } }
  run(state : State) : Result<string> {
    this.pattern.lastIndex = state.position;
    const ms = this.pattern.exec(state.source);
    if (ms) {
      return new ResultOK(state.seek(ms[0].length), ms[0]);
    }
    return new ResultFail<string>(state);
} }

-----

// Parser
apply<B>(f : (a:A) => B) : Parser<B> { return new ApplyP(this, f); }

---------- p.035 List 3.4

class ApplyP<A,B> extends Parser<B> {
  constructor(private p : Parser<A>,
              private f : (a:A) => B) { super(); }
  run(state : State) : Result<B> {
    const r1 = this.p.run(state);
    if (r1 instanceof ResultOK) {
      return new ResultOK(r1.state, this.f(r1.value));
    }
    return r1;
} }

----- List 3.5

class ManyP<A> extends Parser<A[]> {
  constructor(private p : Parser<A>,
              private one : boolean = false) { super(); }
  run(state : State): Result<A[]> {
    const resary : A[] = [];
    let st : State = state;
    for (;;) {
      const res = this.p.run(st);
      if (res instanceof ResultOK) {
        resary.push(res.value);
        st = res.state;
      } else {
        if (this.one && resary.length == 0) {
          return new ResultFail<A[]>(st);
        }
        return new ResultOK<A[]>(st, resary);
} } } }

---------- p.036 List 3.6

class Select<A> extends Parser<A> {
  private ps: Parser<A>[];
  setup(ps: Parser<A>[]) {
    for (const p of ps) {
      if (!p) { throw "Uninitialized Parser"; }
    }
    this.ps = ps;
    return this; // method chain
  }
  run(state: State): Result<A> {
    for (const p of this.ps) {
      const res : Result<A> = p.run(state);
      if (res instanceof ResultOK) { return res; }
    }
    return new ResultFail<A>(state);
} }

-----

many(one : boolean = false) : Parser<A[]> {
  return new ManyP<A>(this, one); }

---------- p.037 List 3.7

class Seq<A> extends Parser<A> {
  constructor(
    private f : {() : IterableIterator<A | Parser<any>>}
  ) { super(); }
  run(state : State) : Result<A> {
    const gen = this.f();
    let res : Result<any> = new ResultOK(state,undefined);
    for (;;) {
      const r : IteratorResult<A | Parser<any>> =
        gen.next((res as ResultOK<any>).value);
      if (r.done) { return new ResultOK(res.state,r.value); }
      res = (r.value as Parser<any>).run(res.state);
      if (!(res instanceof ResultOK)) { return res; }
} } }

-----

if (!p) { throw "Uninitialized Parser passed"; }

---------- p.039

class ProxyP<A> extends Parser<A> {
  private p : Parser<A>;
  setup(p : Parser<A>) { this.p = p; }
  run(s : State) { return this.p.run(s); }
}
