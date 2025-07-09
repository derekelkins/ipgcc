An implementation of a parser generator for Interval Parsing Grammars (IPGs)
roughly as described in [Interval Parsing Grammars for File Format Parsing](https://doi.org/10.1145/3591264)
by Zhang, Morrisett, and Tan.

See the [Tutorial](Tutorial.md), for an overview and introduction to using
the system.

These grammars are geared to binary formats where random access is often
used. Combined with some other features, this provides a powerful language
for handling parsing which is still declarative and analyzable. It can
handle common patterns that most other (truly) declarative parsers cannot.
It's also more principled than "semi-declarative" parsers, such as
[Kaitai struct](https://kaitai.io/) or [binary-parser](https://github.com/keichi/binary-parser/),
which have imperative "seek" operations which complicate analysis.

This does not completely follow the syntax that is described in the paper
or in the accompanying [code artifact](https://zenodo.org/records/7811236).
I also have some important additions. Namely, parameterized rules and
repetition syntax. The parameterized rules are not restricted to statically
determined values which adds significant power. I tend to avoid such uses as
the nice thing about a defined grammar language is a defined level of power,
but you do you. Parameterized rules, even restricted to statically determined
values, are handy for factoring out repeated patterns.

Repetition, as alluded to in the paper, is a necessary addition as otherwise
a (straightforward) translation to a recursive descent parser will produce code
that uses stack linearly.

>  For the largest test file (over 25MB), the IPG parser performs much worse
>  because parsing symbol names requires deep recursion in the IPG parser, which
>  could be eliminated by introducing the Kleene-star operator into IPGs.

I don't (currently) support the `switch` syntax or existentials. I may add the
former in the future, but it doesn't really add that much convenience. The latter
can handled by the `A.these` notation and an external function. I also
don't have the `btoi` expression they reference. Instead, you can use
`{ bs = *[l, r] }` to bind `bs` to a slice of the input from `l` to `r`.
You can then compute a numeric value with an expression.

I did support local rules, i.e. `where` clauses, but I removed that support.
Other than scoping the rule name itself, parameterized
rules allow the same functionality in a much clearer and cleaner fashion.

I do support interval inference.

I currently do not do any analysis, e.g. termination analysis on the grammar.
(In fact, I currently don't even do basic checks like ensuring a used rule is
defined, etc., but this will probably change.)

TODO: Reconsider the parameterized rule syntax and/or the syntax for
referencing an "array" element.

## Syntax Overview

WARNING: The concrete syntax is subject to change. I'm not 100% happy with
some of the choices made so far.

To just give a flavor, here's a snippet from the (partial) ELF parse in the tests
which illustrates some of the unique features. This parses a header (the `H` rule
isn't listed here) which gives the offsets and sizes to a collection of section
headers. The section headers then give the offsets and sizes of the actual sections.
The `Sec` rule, which parses a section, is parameterized by the type of section and
performs a switch via a sequence of guarded alternatives. In the default case at the
end, we just take the entire buffer (`*[0, EOI]`) but here the "entire buffer" is
relative to the interval `Sec` was called on. This example illustrates random
access into the input and, potentially, [multiple passes over the same data](https://nathanotterness.com/2021/10/tiny_elf_modernized.html).

```
ELF -> H[0, 128]
       for i = 0 to H.e_shnum do
           SH[H.e_shoff + i*H.e_shentsize, H.e_shoff + (i + 1)*H.e_shentsize]
       for i = 1 to H.e_shnum do
           Sec(SH(i).sh_type)[SH(i).sh_offset, SH(i).sh_offset + SH(i).sh_size]
       { header = H.this }
       { section_headers = SH.these }
       { sections = projectSections(Sec.these) };

Sec(sh_type)
    -> ?[ sh_type == 6 ]
       DynSec { section = DynSec.section }
     / ?[ sh_type == 3 ]
       StrSec { section = StrSec.section }
     / ?[ sh_type == 11 || sh_type == 2 ]
       DynSymSec { section = DynSymSec.section }
     / ?[ sh_type == 7 ]
       NoteSec { section = NoteSec.section }
     / ?[ sh_type == 9 ]
       RelSec { section = RelSec.section }
     / ?[ sh_type == 4 ]
       RelAddEndSec { section = RelAddEndSec.section }
     / ?[ sh_type == 8 ] // NoBits section
       { section = empty() }
     / { section = *[0, EOI] };
```

The following uses a BNF-like description of syntax rules, and regexes for terminals.

- `--` indicates a line comment.
- `P?` indicates an optional `P`.
- `P*` indicates zero or more repetitions of `P`.
- `P+` indicates one or more repetitions of `P`.
- `|` indicates alternation.
- Terminals are wrapped in quotes.
- Parentheses can be used for grouping.

```bnf
-- Regexes describing more complicated terminals.
NAME = /[_a-zA-Z][_a-zA-Z0-9]*/
BOOL = (true|false)
INT = /[0-9]+/
FLOAT = /[0-9]+.[0-9]*/
STRING = /"([^\n\\"]|\\[0abfnrtv\\"']|\\x[0-9a-fA-F][0-9a-fA-F])*"/
NT = /[_a-zA-Z][_a-zA-Z0-9]*(@[0-9]+)?/
-- NT is a NAME optionally followed by a disambiguating number of the form A@123

Grammar ::= RuleOrConst*

RuleOrConst ::= Rule | Const

-- const foo = 10;
Const ::= "const" NAME "=" Exp ";"

-- Schematic Example: A(a_1, ..., a_m) -> alt_1 / ... / alt_n;
Rule ::= MetaTags* NAME ParameterList? "->" Alt ("/" Alt)* ";"

MetaTags ::= "%instrument" | "%export"

ParameterList
  ::= "(" ")"
    | "(" NAME ("," NAME)* ")"

Alt ::= Term+

Term
  ::= NT ArgumentList? Interval?    -- A@1(a_1, ..., a_m)[l, r]
    | STRING Interval?              -- "foo"[l, r]
    | "{" NAME "=" RHS "}"          -- { id = e }
    | "?[" Exp "]"                  -- ?[ e ]
    | "for" NAME "=" Exp "to" Exp "do" NT ArgumentList? Interval?
      -- for id=e_1 to e_2 do A@1(a_1, ..., a_m)[e_l, e_r]
    | "repeat" NT ArgumentList? Interval? "." NAME
          ("starting" "on" Interval)?
      -- repeat A@1(a_1, ..., a_m)[l, r].id starting on [l0, r0]
    | "repeat" NT ArgumentList? Interval? "." NAME
          ("starting" "on" Interval)? "until" NT ArgumentList?
      -- repeat A@1(a_1, ..., a_m)[l, r].id starting on [l0, r0] until B@1(b_1, ..., b_k)

RHS
  ::= Exp                 -- e, as in { id = e }
    | "." ("[" Exp "]")?  -- .[e], as in { id = .[e] }
    | "*" Interval?       -- *[e], as in { id = *[l, r] }

ArgumentList
  ::= "(" ")"
    | "(" Exp ("," Exp)* ")"

Interval
  ::= "[" Exp "]"
    | "[" Exp "," Exp "]"

-- See below for the precedences, but they are intended to follow JavaScript.
Exp ::= BOOL                      -- true
      | INT                       -- 123
      | FLOAT                     -- 123.5
      | STRING                    -- "foo"
      | Exp BinOp Exp             -- x + y
      | UnOp Exp                  -- -x
      | Exp "?" Exp ":" Exp       -- b ? t : e
      | NAME ArgumentList         -- f(x, y)
      | Exp "[" Exp "]"           -- a[i]
      | NAME                      -- id
      | NT "." NAME               -- A@1.id
      | NT "(" Exp ")" "." NAME   -- A@1(e).id
      | "(" Exp ")"               -- (x)

BinOp ::= "||" | "&&" | "|" | "^" | "&" | "==" | "!=" | "<" | ">" | "<=" | ">="
        | "<<" | ">>" | "+" | "-" | "*" | "/" | "%" | "**"

UnOp ::= "-" | "+" | "~" | "!"
```

The precedences for the expressions from lowest to highest follow JavaScript and,
in Happy notation,  are:

```
%right '=' '?'              -- 2
%left '||'                  -- 3
%left '&&'                  -- 4
%left '|'                   -- 5
%left '^'                   -- 6
%left '&'                   -- 7
%left '==' '!='             -- 8
%left '<' '>' '<=' '>='     -- 9
%left '<<' '>>'             -- 10
%left '+' '-'               -- 11
%left '*' '/' '%'           -- 12
%right '**'                 -- 13
%nonassoc NEG PLUS '~' '!'  -- 14
%nonassoc '['               -- 17
%left '.'                   -- 17
```

C-style comments are supported.

`EOI` as a stand-alone expression is treated specially. It should not be used as a
parameter name. You could use it as an attribute or rule name if you like.
Incidentally, "EOI" stands for "end of input" and refers to the end of the slice
given to the currently executing rule.

Special fields:
- `A.START` - the beginning of the interval actually parsed by `A` most recently
- `A.END` - the end of the interval actually parsed by `A` most recently
- `A.this` - the collection of attributes associated to `A` returned as an object
- `A(e).this` - the collection of attributes associated to `A(e)` returned as an object
- `A.these` - if `A` was most recently used in an array expression, this will be an
              array of objects representing the collection of attributes for each entry

These identifiers should not be used as names of attributes. You could use them as
parameter or rule names, though using `this` in such a way would cause issues for
the JavaScript back-end.

The expression syntax is geared to cover common use-cases in parsing binary files
while still being generic enough to be straightforwardly translated to a variety
of target languages. In particular, there is no array or object literal syntax
nor (general) object field selection. Field selection is restricted to attributes
of non-terminals or components of array notation. The intent is that any more
involved or back-end language-specific processing would be handled by external
functions. It is a bit tedious at times though...

`%instrument` marks a rule for debugging instrumentation.

`%export` indicates that the function generated should be marked as exported.

### External Interface

For the purposes of error checking, you can declare some rule names as external
so they will not be treated as missing. You do this with a `%declare` declaration
which is terminated with `%end`. The `%declare` must be at the start of a line and
before the grammar. Between the `%declare` and `%end` is a whitespace separated list
of names.

Example:
```
%declare Foo Bar Baz %end
```

More substantively, you can use the `%preamble_end` and `%postamble_begin` declarations
to add some text to be included in the output. Each of these declarations must occur at
the start of a line. They split the file into (up to) three parts: the preamble,
the part before `%preamble_end`; the postamble, the part occurring after `%postamble_begin`;
and the grammar, which occurs between them. Either the preamble and the postamble
can be omitted along with the corresponding declaration. Note, a `%declare` goes
in the grammar part, not the preamble.

Other than removing leading white space from the postamble, the preamble and postamble
are included in the generated file AS-IS with the preamble occurring before
the generated code and the postamble following it.

## Semantics

Each rule is invoked against an interval specifying a slice of the input.
Any attempt to access outside of that interval causes the parser to fail.
An **invalid interval** is one where the start is *strictly* after the end,
i.e. for `[l, r]` we have `l > r`. Empty intervals, i.e. where `l == r` are
allowed and useful. Nothing stops having different terms parse overlapping
slices of the input. In fact, this is a powerful technique that allows
multi-pass parsing. (See [`test/node/test-elf.ipg`](test/node/test-elf.ipg)
for an example.)

In a way similar to [Parsing Expression Grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar)
(PEGs), we have a *biased* alternation operator, `/`. Here, the first alternative
to succeed is taken and other alternatives will not be considered. In other
words, the resulting parser is deterministic in that it will produce at most one
successful parse. Terms within an alternative are executed in data dependency
order and then in written order and must all succeed for the alternative to succeed.

The expressions, `Exp` above, behave as you expect. The syntax and semantics are
taken from JavaScript, though likely generators will use the semantics details of the
target language. (That does make a grammar potentially back-end dependent. For
example, many languages have different meanings for the modulus operation, `%`.
I don't mean that `%` isn't modulus in some languages. Back-ends should always
translate `%` into a modulus operation. I mean that languages differ in what the
modulus operation should do, particularly in regards to handling negative inputs.)
The main notable expressions are `A.id`, `A(e).id`, and `id`. These largely
behave as you expect. The first accesses the attribute associated to the *most
recently preceding* invocation of the non-terminal `A`. `A(e).id` is similar
but it applies to the most recent array term involving `A` and access the `id`
attribute of the `e`-th element of that sequence. While it's currently unchecked,
attempting to access an out of bounds element of the sequence will lead to failure.
(TODO: I'm not sure if it should just be parse failure or a panic.)
Finally, `id` references either a parameter or an attribute on the current rule.
Currently, it will always prefer a parameter, but I may change it to allow attributes
to shadow parameters in following terms.

TODO: Check out-of-bounds sequence accesses.

Most of the actual *novel* semantics is due to the terms.

### Terms

Many terms take an interval, `[l, r]`, which then specifies which slice of the input
they process. Part or all of the interval can be omitted and interval inference,
described in the next section, will fill in the details, so I will assume we have
full intervals in the following.

Attempting to invoke a term with an invalid interval, i.e. where `l > r`, immediately
fails. Note, *empty* intervals, where `l == r`, are not invalid. Similarly,
attempting to invoke a term with a (partially) out-of-bounds interval leads to
the current rule failing. This is a totally normal occurrence. For example,
if you wanted to parse 5-byte blocks until the end of input

Non-terminal invocations, e.g. `A(x, y)[l, r]`, simply invoke the corresponding
rule on the slice of the input indicated by the interval. Arguments, if any,
will be evaluated in the context of the current rule and passed exactly as you
would expect. After this invocation, `A` is bound and its attributes can be
accessed in following terms. If `A` was already bound, it is shadowed.

Terminals, e.g. `"foo"[l, r]`, succeed if the given string is a *prefix* of the
corresponding slice of the input. It fails if the terminal doesn't match or is
longer than the slice.

Guard terms, e.g. `?[ e ]`, evaluate the argument and succeed or fail based on
whether the argument is true. (The JavaScript back-end will use JavaScript's
"truthiness" semantics, but grammars [and JavaScript programmers for that matter...]
should avoid relying on such behavior for portability.) This term consumes
no input.

Array terms, e.g. `for id=e_1 to e_2 do A(a_1, ..., a_m)[e_l, e_r]`,
iterates from `e_1` to `e_2` invoking the non-terminal term in the body
with `id` bound to the current iteration value. Notably, `id` is bound
in `e_l` and `e_r`, i.e. the interval can depend on the iteration variable.
All iterations must succeed for this term to succeed.
After an array term, `A` is bound and attributes of its elements can be
accessed via the `A(e).id` expression. Also, `A.START` and `A.END` are
meaningful and will refer to their values in the last iteration. `A.id`
is otherwise invalid. (TODO: Maybe allow this and have it refer to the
last iteration? It would slightly simplify the JavaScript translator
and may be useful upon occasion.)

Importantly, I allow `e_l` and `e_r` to themselves refer to `A.START`
and `A.END`. In this case, they will be bound to the values for the
previous iteration. For the first iteration, they will be bound to
the equivalent of `Prev.START` and `Prev.END` where `Prev` is the previous
non-terminal, or similar (see the interval inference) for terminals.
They will be `EOI` and `0` respectively if there is no previous term.

**WARNING**: The `A(e).id` notation uses the same indexing as the for loop.
With `for i = 10 to 20 do A[l, r]`, `A(j).id` is valid for `j` from 10
to 19, but `A.these` is an array with 10 elements indexed from 0. It is
probably best to endeavor to start the loops at 0. I may even remove
the ability to specify a start, perhaps using a syntax like
`for i upto 20 do A[l, r]`.

Assignment terms come in three versions: `{ id = *[l, r] }`, `{ id = .[l] }`,
and `{ id = e }` for an arbitrary expression `e`. The first two consume
input while the last does not.

`{ id = *[l, r] }` binds the attribute `id` to the slice of the input indicated
by the interval. It will fail, like any term, if the interval is invalid or
(partially) out-of-bounds.

`{ id = .[l] }` binds the attribute `id` to the value of the input at `l`.
It is essentially equivalent to `{ tmp = *[l, l + 1] } { id = tmp[0] }`.

`{ id = e }` simply evaluates the expression `e` and binds it to the attribute
`id`. It consumes no input and can never fail. Note that `*[l, r]` and `.[l]`
are not expressions, so you can't do something like `{ id = process(*[l, r]) }`.

The semantics of `repeat` and `repeat-until` are straightforward as they exist
to optimize a common pattern.

Namely, `repeat A(a_1, ..., a_m)[l, r].id starting on [l0, r0]` is logically
shorthand for invoking the following rule as `R[l0, r0]` in the current scope:

```
R -> A(a_1, ..., a_m) R[l, r] { values = cons(A.id, R.values) }
   / { values = nil() };
```

and similarly for `repeat A(a_1, ..., a_m)[l, r].id starting on [l0, r0] until B(b_1, ..., b_k)`:

```
R -> B(b_1, ..., b_k) { values = nil() }
   / A(a_1, ..., a_m) R[l, r] { values = cons(A.id, R.values) };
```

Here `cons` and `nil` are functions that append an element to the beginning of
an array and create an empty array respectively.

After the repeat/repeat-until term, `A.values` holds an array of the indicated
values.

These terms should always be preferred over a recursion as the recursive versions
consume call stack (not just because JavaScript doesn't have tail-call elimination).
If more flexibility is needed, you could fall back to a recursive version as
long as you don't expect too many iterations.

### Interval Inference

Interval inference is done as described in the paper. The paper doesn't actually
describe the array case, i.e. `for`. Since I allow `A.START`/`A.END` rules following
an array term can use the same rules as if they followed a plain non-terminal case.

In a nutshell, for a non-terminal `A`, the term `A` becomes `A[Prev.END, EOI]` where `Prev`
is the non-terminal immediately preceding `A`. If a terminal immediately precedes `A`,
then instead of `Prev.END` we'll get the start of the terminal's interval plus the length
of the terminal as the start of `A`'s interval. Note that this is not the same as the
*end* of the terminal's interval. If `A` is the first term, `Prev.END` will be 0.

`A[e]` becomes `A[Prev.END, Prev.END + e]`, i.e. when only one component of the interval is
given, it indicates a length.

For terminals, we know the length so `"s"[l]` becomes `"s"[l, l + "s".length]`.
Just `"s"` can then become `"s"[Prev.END]` and then be transformed again where
`Prev.END` is as above.

For `repeat`/`repeat-until` terms, interval inference proceeds like above for the
`starting on` portion where an omitted `starting on` behaves like `starting on` with
no interval. For the body, have `repeat A.id` expands to `repeat A[A.END, EOI].id`
and `repeat A[l].id` expands to `repeat A[A.END, A.END + l]` and similarly for
`repeat-until`.

Interval inference proceeds left-to-right.

## Tricks

### Skipping input

This is more of a warning than a trick. DON'T use `{ _ = *[l] }` or similar to
skip past input. You can just give the next term an interval like `[Prev.END + l, EOI]`
instead. This will avoid "reading" that input unnecessarily, and this kind of thing
is one of the strengths of IPG.

### Backward Parsing

Since we're specifying intervals we don't need to have the terms in the order
they occur in the file. Still, it would be nice in most cases to write the terms
in the order they occur in the file, even if, for whatever reason, we are not
parsing them in that order.

If we have a `B` at the end of the input, and an `A` immediately preceding it, but
we have to parse `B` to know where it begins, we can write:

```
S -> A[0, B.START] B[0, EOI]
```

This will parse `B` to figure out where `B.START` is and then parse `A`. If we wanted
the more operational order, we can write it the other way which is completely equivalent.

```
S -> B[0, EOI] A[0, B.START]
```

`S -> A[0, B.START] B[A.END, EOI]` is disallowed since it's ambiguous whether we
should parse `A` first or `B`. To be precise, the data dependencies between terms
determine the parse order, and this example has a cyclic dependency graph.

### Avoiding END calculations

`{ x = .[e] }` and `{ y = *[l, r] }` consume input but don't give you a name to
refer to with `.END`. This isn't usually a problem since you can typically rely
on interval inference. When you can't, you have a couple of options:

1. compute the value yourself,
2. wrap the term in a non-terminal, or
3. use a non-terminal that doesn't consume input.

For the last, we could write something like `Marker -> ?[1];` and then
`{ x = *[e] } A[???, EOI]` could become `{ x = *[e] } Marker A[Marker.END, EOI]`.

### Keeping the attribute sets clean

I like to avoid returning attributes that are just there as helpers and don't
reflect the logical output. Since the only attributes that are "returned" are
those explicitly assigned in the rule, you can use a subrule to compute the
attributes and then the main rule can just return the relevant attributes. For example:

```
MainRule -> SubRule { value = SubRule.value };
SubRule -> A { x = A.y } { b = . } { value = compute(x, b) };
```

In particular, `{ id = . }` and `{ id = *[l, r] }` lead to this situation often.
I often use: `U8 -> { value = . };` to parse a byte with an unnamed value. For
slices, `B(n) -> { value = *[n] };` could be used.

### Rule that only accepts empty input

```
Empty -> ?[ EOI == 0 ];
```

is a rule that succeeds if and only if it is evaluated on the empty interval.
It can be used as `repeat A.id until Empty` to keep applying `A` until there
is no input remaining. That said, this is not the same as applying `A` until
it fails which is what `repeat A.id` does. `repeat A.id until Empty` only
succeeds if repeated invocations of `A` will consume *exactly* all the input.

## JavaScript Output

A rule in the grammar gives rise to a JavaScript function of the same name.
This function takes in either a string or an array of numbers &ndash; typically a
`Uint8Array` &ndash;, then two integers indicating the start and end of the slice
to use, and it outputs an object or `null` in the case of parse failure.
In addition to any attributes defined in the rule, which will occur as fields of
the same name on the returned object, there are the fields `_ipg_start` and `_ipg_end`.
`_ipg_start` and `_ipg_end` give the start and end points of the interval actually
parsed. If no data was consumed, `_ipg_end` will be `0` and `_ipg_start` will be the
length of the input.

As a TypeScript declaration:

```typescript
interface ParserOutput {
  _ipg_start: number,
  _ipg_end: number,
  [id: string]: unknown, // All the attributes
}

function MyRule(
  input: string | Uint8Array,
  start: number = 0,
  end: number = 0
): ParserOutput | null;
```

Any function that has an interface similar to this can be used as
a parser. You can just reference that function as a non-terminal in your parser.

If `--async-mode` is passed, an asynchronous interface is expected and produced.
See [`docs/AsyncMode.md`](docs/AsyncMode.md).

The output is a straightforward recursive descent parser *without* packrat-style
memoization.

The JavaScript isn't optimized. While it should be reasonably efficient, there are many
ways it could be improved.

- Objects holding the collections of attributes could be reused more. Currently,
  I discard them as a quick way of removing obsolete attributes. Reusing these,
  would likely use more memory as garbage would survive longer.
- The prefix check for non-terminals uses the function `_ipg_startsWith` which is
  naively implemented. The generator knowing the type of the input could allow a
  good implementation to be chosen. (Feel free to replace it in the output with
  something better or more specialized.)

I'm not worried about things like constant expressions. I assume the JavaScript
implementation (or any back-end for a well optimized language) will handle this
easily. I do apply some simplification to the expressions, but mostly to make the
output more readable for my own sanity.

There are also potentially grammar-level optimizations that could be done, e.g.
recognizing a pattern of recursion as a repetition, though I don't see implementing
any of these unless some very compelling case comes up.

I do recommend having parsers for primitive types be implemented via external
parsers. See [test/node/test-rm6.ipg](test/node/test-rm6.ipg) for an example.
Specifically, the `Float32`/`Float64` parsers.

TODO: Describe debug mode.

### Reserved Identifiers

The following names should not be used in your grammar to avoid conflicting with the
implementation.

Field names used by the generator: `_ipg_start`, `_ipg_end`

Function names used by the generator: `_ipg_startsWith`

### Input Requirements

Here are the requirements on the input when it is not a string.

- It must have a `length` field that behaves as usual. (This can be omitted if
  you don't rely on the default values when you call a rule function.)
- Array index notation should work on it, e.g. `input[i]`. This should return
  a number. It will be compared against `s.charCodeAt(i)` for terminals.
- It should have a `slice` function that behaves as usual. This slice function
  will only ever be called with two arguments. The output of this slice function
  must obey the same requirements as here (or be a string). (The `slice` function
  is only used for `{ id = *[l, r] }`, so it can be omitted if you don't use that.)

## Examples

See the `test/*/` directories for some non-trivial example grammars. The examples
in `test/node/`, in particular, contain JavaScript code in the preamble/postamble
producing self-contained executable examples.

### ELF

[`test/node/test-elf.ipg`](test/node/test-elf.ipg) is a translation of the ELF parser
from the paper's code artifacts. It does not handle the fullness of [ELF](https://www.man7.org/linux/man-pages/man5/elf.5.html),
but it is a good illustration of a file format that significantly leveraged
random access.

My adaptations are fairly straightforward and mostly involve replacing uses
of `btoi` and using the repetition terms I added as the recursive version
leads to stack overflows even on the example ELF files they used.

### GIF

[`test/node/test-gif.ipg`](test/node/test-gif.ipg) is a full [GIF89a](https://giflib.sourceforge.net/gifstandard/GIF89a.html)
parser. Originally, I started translating the example from the code artifact,
but some ambiguities and incompleteness led me to look at the GIF spec. At that
point I decided just implementing it from scratch and more closely following the
spec would be easier and more compelling. The result is, indeed, compellingly
straightforward.

As an example of using IPG, GIF is an example of a chunk-based format.

### ISO9660

[ISO9660](https://ecma-international.org/publications-and-standards/standards/ecma-119/) is
one of the specifications that governs the format of CD-ROM file systems. Or, nowadays,
`.iso` files.

This example illustrates traversing a tree-like structure specified by absolute offsets.
ISO9660 also specifies a flat version of the directory structure with the Path Tables.
This parser illustrates traversing both.

[This live example](https://www.hedonisticlearning.com/ipgcc/docs/AsyncExample/viewer.html)
illustrates this. The example uses the `--async-mode` output to only fetch the
parts of the ISO that are needed.

### QOI

I made a [QOI](https://qoiformat.org/) parser as an early simple but "real" example.
Unfortunately, it doesn't really leverage anything special about IPG.

Nevertheless, [`test/node/test-qoi-syntax.ipg`](test/node/test-qoi-syntax.ipg)
provides a parser that will output the structure of a QOI file. To make it more
interesting and better validate the code, [`test/parsing/test-qoi.ipg`](test/parsing/test-qoi.ipg)
actually produces a full QOI to PNG converter. You can
see a variant of this in action at [`docs/QOI.html`](https://www.hedonisticlearning.com/ipgcc/docs/QOI.html).
The parser itself passes through a `state` object via parameterized rules which maintains the
decoders state. This is updated by functions called in assignment terms in the
grammar. In theory, using impure functions like this as expressions is ill-defined.
In practice, for a PEG-like grammar language and a recursive descent based implementation,
when and how often an expression will be evaluated is fairly predictable and
understandable.

The parser produces a sequence of color objects.

### RM6

This is a parser for the reMarkable tablet's V6 lines file. This was the motivation
for me creating this tool. I wanted to be able to convert these files to SVG in the
browser. There's code that does this... in Python. So I started looking at writing a
parser of my own. Looking at the latest tools and literature on declarative parsing
led me to Interval Parsing Grammars. In this case using some kind of "declarative"
parsing approach wasn't just a (strong) preference of mine, but I also wanted to have
an artifact that would describe the format at a reasonably high level. This format is
not documented by the producer and so has been reversed engineered. However, the
known public information on the format seems to be largely represented by two codebases
that provide little documentation beyond the code itself.

My hope is that even without comments of its own, the grammar here can give a relatively
high-level overview of at least the syntactic aspects of this format.

As an example, there isn't too much novel about it other than its size and some checking
code incorporated into it.

## Future Work

### Not Future Work

First, some things that are extremely unlikely to be added. This formalism is geared
toward binary formats. While you technically *can* make a parser for a programming
language or whatever with this, there is no benefit to doing that in IPG as compared
to other grammar formalisms such as PEG. As such, things like being able to specify
precedence of operators won't be added. Similarly, "good error messages" isn't
really a goal, however see the section on [Grammar Debugging Tools](#grammar-debugging-tools).

### Imports

TODO: Add some kind of import mechanism. For now, CPP is probably the simplest solution.

### Termination Checker and Other Analyses

The paper discusses termination checking. There are other potential static analyses
that could be done. One of the benefits of using a grammar formalism is that it
is relatively easy to analyze compared to arbitrary code.

### Grammar Debugging Tools

I've made some stabs at debugging aids, but I'm not really sure what is useful (except
that I wish I implemented the validation earlier). Currently, `%instrument` just
spits out the result of a successful rule application, but that is mostly useful
for indicating that some subparse succeeded and had the values you expect. If it
doesn't though, it doesn't really help you know why. There's also a "debug mode"
that, currently, produces a tree-like structure which explains why a parse failed.
Specifically, it specifies what term failed for each alternative of a rule that
failed and the relevant interval. This is most useful when the entire parse failed,
but many rules always succeed, e.g. `repeat A.id` always succeeds.

The good-ish news is that a recursive descent parser has a close and straightforward
relationship to the grammar, so debugging the generated code with JavaScript debugging
tools is a completely reasonable thing to do.

### Static Types

Adding types and type checking would be nice both to catch more errors in the grammar
and to ease export to statically typed languages.

### More Back-ends

I don't really have a need for other back-ends, but it would be nice to have others.
Allocation is more or less unavoidable to represent the collections of attributes,
but otherwise it should be fairly straightforward to adapt the JavaScript export to
other languages. One could also imagine an optimizing version which, for example,
inlines rules.

### Streaming

The nature of the grammar formalism implies that streaming isn't an option in general.
You can trivially (and usefully!) have a rule that checks the end of the input as
the first thing it does. Indeed, this is what the PDF parser in the paper does. Nevertheless,
the paper suggests as future work of its own, that you could make a static analysis that
could determine when an IPG grammar was streamable.

Possibly you could add a variant of `EOI` called `UNKNOWN` representing the remainder
of the input. The difference would be that `UNKNOWN` isn't a number, so you can't do
arithmetic on it, `UNKNOWN` can never be the start of an interval, and `EOI` is only
defined if the end of the interval a rule was applied on was a number, i.e. not `UNKNOWN`.
The simplest approach would just enforce these checks at runtime.

### Tail-Call Elimination

The `repeat` and `repeat-until` story is only necessary because recursion leads to
stack overflows. This isn't just a problem with the underlying runtime, i.e. this
isn't just because JavaScript doesn't guarantee tail-call elimination (though that
is also an issue.) The issue is two-fold. First, there is some work maintaining
`A.START` and `A.END` and checking for failure that happens after a rule completes.
Second, you often do want to do something after what would otherwise be a tail call.
Most fundamentally, you probably want to assign the result to some attribute.

Adding notation like `A(a_1, ..., a_n)[l, r].x into y` that is conceptually the
same as `A(a_1, ..., a_n)[l, r] { y = A.x }` and then using continuation-/destination-passing
style techniques could probably resolve the issues. (At which point we'd run into
the issue of the underlying runtime not supporting tail-call elimination.)
Even for non-tail-calls this is a common pattern, so this notation would be nice.

### Higher-Order Rules

You can almost sort of already do this kind of by accident in the JavaScript export, but
being more deliberate about this may be nice. That said, it would be very easy to make the
grammar significantly less analyzable this way, so one could imagine this being guarded
by a flag.
