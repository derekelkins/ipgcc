# Overview

Interval Parsing Grammars (IPGs) is a grammar formalism geared to binary formats.
If you've used Parsing Expression Grammars (PEGs), this has some similarities in
that it has a biased alternation compared to the unbiased alternation of Context-Free
Grammars (CFGs). I'm assuming some exposure to CFGs, PEGs, or similar formalisms.

If you are very familiar with PEGs and CFGs, then you can skim the following to
familiarize yourself with the notational choices here up until the [Intervals](#intervals)
section which introduces the main difference compared to PEGs and CFGs.
The earlier sections are intentionally written to only cover the "usual"
concepts of grammars.

## Supporting Code

For the JavaScript export (or future compiled back-ends), you can include code in
the output by adding it to the preamble and postamble. The preamble is anything
before a line starting with `%preamble_end`. The postamble is anything following
a line starting with `%postamble_begin`. If one or both of these markers aren't
in the input file, then there is no preamble or postamble respectively. The grammar
is contained between these markers (if they are present). The preamble and postamble
are included into the final output as-is.

## Running

In most cases, after building the project, e.g. via `cabal install`, you would
run via:

```console
$ ipgcc -i input.ipg -o generated.js
```

If you want to use the interpreter, you would use:

```console
$ ipgcc -i input.ipg -I < example.bin
```

You can use:

```console
$ ipgcc -i input.ipg -t CORE -o core.ipg
```

to get a pretty-printed version of the input after some preprocessing
such as interval inference and non-terminal disambiguation.

## Basics

Starting with the parts similar to PEGs or CFGs, a grammar consists of a series
of rules written as:

```ipg
R -> A1 / A2 / A3;
```

where `R` is a non-terminal called the **head of the rule** and `A1` through `A3`
are alternatives which are space separated sequences of terms which we'll go into
more detail later. Unlike CFGs, a non-terminal can only be the head of one rule.
In other words,

```ipg
R -> A1;
R -> A2;
R -> A3;
```

is disallowed.

Like PEGs and unlike CFGs, the alternatives are tried in order and the first one
that succeeds is chosen.

Terminals are represented by literal strings. So to recreate a standard example:

<small>[test/interpret/test-tutorial-1.ipg](test/interpret/test-tutorial-1.ipg)</small>
```ipg
Exp -> Term "+" Exp / Term;
Term -> Factor "*" Term / Factor;
Factor -> "(" Exp ")"
  / "0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9";
```

`Exp` then matches simple arithmetic expressions involving addition and multiplication
which will both be right associated in the above. (In this case, the numbers are
only single digit.) Due to the PEG-like biased choice,
it will always attempt to parse the longest `Exp` and similarly the longest `Term`.
Unlike CFGs, there is never any ambiguity. There is at most one successful parse.

## Terms

### Guards and Expressions

Like PEGs, IPG supports guards which cause an alternative to fail based on a condition.
These are written as `?[ e ]` where `e` is an expression. There expression syntax is
described in the [README](README.md), but is a fairly typical C-style expression syntax.
The exact details on precedence and such are taken from JavaScript. It supports typical
arithmetic, bitwise, and relational operators as well as the ternary operator (`b ? t : e`)
and function call syntax. Indexing into sequences and strings is possible with the
usual `xs[i]` syntax. To make it relatively generic, it doesn't support things like
JavaScript's array or object literal notation nor field selection. The intent is that
you can wrap any back-end specific code in a function and call that.

You can name constants to be used across many rules via `const SOME_CONST = e;` where
`e` is an expression. This expression can't use `EOI` or refer to any rules. It can
refer to other constants including constants declared later.

### Attributes

Of course, to do anything interesting we need something for the expressions to manipulate.
To this end, there are attribute defining terms of the form `{ x = e }` which binds the
attribute `x` to the value of the expression `e`. There is also `{ x = . }` which binds
`x` to the next byte parsed. `{ x = *[l] }` binds `x` to the sequence of bytes of the next
`l` bytes. Here `l` is an arbitrary expression.

As an example, `{ x = . }` is equivalent to `{ y = *[1] } { x = y[0] }` (except that
it also defines the attribute `y`).

Attributes are scoped to an alternative, so:

```ipg
S -> { x = 10 } / { y = 2 * x };
```

is illegal.

Attributes defined in a rule can be accessed by other rules which invoke this one.
However, only attributes that are defined in every alternative are available.
Expanding the earlier example to return a result, we have:

<small>[test/interpret/test-tutorial-2.ipg](test/interpret/test-tutorial-2.ipg)</small>
```ipg
Exp
  -> Term "+" Exp { value = Term.value + Exp.value }
   / Term { value = Term.value };

Term
  -> Factor "*" Term { value = Factor.value * Term.value }
   / Factor { value = Factor.value };

Factor
  -> "(" Exp ")" { value = Exp.value }
   / "0" { value = 0 }
   / "1" { value = 1 }
   / "2" { value = 2 }
   / "3" { value = 3 }
   / "4" { value = 4 }
   / "5" { value = 5 }
   / "6" { value = 6 }
   / "7" { value = 7 }
   / "8" { value = 8 }
   / "9" { value = 9 };
```
Now when parsing `Exp` we'll end up with an object with a `value` attribute
set to the value of evaluating the expression.

Here we see that `R.x` where `R` is a non-terminal and `x` is an attribute is
another type of expression. The special attribute `this` allows returning
the collection of attributes as an *opaque* (to IPG) object. In the JavaScript
back-end, it will be a JavaScript object, but `{ x = A.this } { y = x.a }` is
illegal syntax since `x` isn't a non-terminal. You can access the fields in
external functions, but those will be back-end-specific. You could, for example,
define a function `lookup` such that `{ x = A.this } { y = lookup(x, "a") }`
accomplished what the above was trying to do if you wanted.

The order of the terms is not necessarily the
order in which the terms will be evaluated. Instead, they will follow data
dependencies. That means, something silly like:

```ipg
Exp
  -> Term { value = Term.value + Exp.value } "+" Exp 
   / { value = Term.value } Term;
```

is exactly equivalent to the first example. That said, cyclic dependencies
aren't allowed and there are also some implicit dependencies which we'll talk
about shortly. Also, some ambiguous cases will be resolved by assuming you
intend the most recently preceding relevant term. For example, in:

```ipg
S1 -> Exp Exp { result = Exp.value } Exp;
S2 -> Exp { result = Exp.value } Exp Exp;
```

`S1` and `S2` are *not* equivalent. In `S1`, `result` will refer to the `value`
attribute of the second occurrence of `Exp`, and it `S2` to the first.

As I've implemented it, an IPG does *not* automatically return some kind of
parse tree. In the first definition of `Exp` with no attribute definitions,
the grammar would match the relevant arithmetic expression, but the output
would just be the fact that it matched and the range of the input that matched.
If you want some kind of syntax tree, you need to explicitly build it yourself
via attributes.

### Disambiguation

If needed to make things clear, you can append `@n` where `n` is a number
to a non-terminal name to disambiguate. No space is allowed between the non-terminal
the `@` and the number. For example, `S1` and `S2` *do* mean the same thing in the
following example.

```ipg
S1 -> Exp Exp@1 { result = Exp@1.value } Exp;
S2 -> Exp { result = Exp@1.value } Exp@1 Exp;
```

The `R@n` notation is called an alias and behaves exactly like `R`, it just has
a different name.

### Repetition

While you could implement repetition via recursion, deep recursion can lead to
poor performance and stack overflows. To this end, there are `repeat` and `repeat-until`
terms.

The term `repeat A.x` invokes `A` repeatedly until it fails. The value of the attribute
`x` is collected after each repetition. `repeat A.x` binds a `values` attribute with
the sequence of values of `x`. This behaves like the Kleene star.

If we wanted to handle many digits in our expression parser, we could rewrite `Factor`
as:

<small>[test/parsing/test-tutorial-3.ipg](test/parsing/test-tutorial-3.ipg)</small>
```ipg
Factor
  -> "(" Exp ")" { value = Exp.value }
   / Number { value = Number.value };

Number
  -> Digit { leadingDigit = Digit.value }
     ?[ leadingDigit != 0 ]
     repeat Digit.value
     { value = digitsToNum(leadingDigit, Digit.values) };

Digit
  -> "0" { value = 0 }
   / "1" { value = 1 }
   / "2" { value = 2 }
   / "3" { value = 3 }
   / "4" { value = 4 }
   / "5" { value = 5 }
   / "6" { value = 6 }
   / "7" { value = 7 }
   / "8" { value = 8 }
   / "9" { value = 9 };
```

where `digitsToNum` is some external function that takes an array of numbers from 0 to 9
and computes a decimal number out of them.

`repeat-until` operates similarly except it terminates the iteration when the second
parser succeeds.

For example:

<small>[test/interpret/test-tutorial-4.ipg](test/interpret/test-tutorial-4.ipg)</small>
```ipg
String -> Quote repeat Char.value until Quote;

Char -> { value = . };

Quote -> "\"";
```

`repeat A.x until B` behaves like invoking the rule `R` defined as:

<small>[test/interpret/test-tutorial-5.ipg](test/interpret/test-tutorial-5.ipg)</small>
```ipg
R -> B { values = nil() }
   / A R { values = cons(A.x, R.values) };
```

where `nil` and `cons` hypothetical functions that build an empty sequence and prepend
to a sequence respectively. The biased choice is important in our `String` example
since `Char` matches quotation marks too.

## Parameterized Rules

Parameterized rules are allowed as well. For example:

<small>[test/interpret/test-tutorial-6.ipg](test/interpret/test-tutorial-6.ipg)</small>
```ipg
const BASE = 10;

Number -> Digit ?[ Digit.value != 0 ] Decimal(Digit.value) { value = Decimal.value };

Decimal(accumulator)
  -> Digit Decimal(BASE*accumulator + Digit.value) { value = Decimal.value }
   / { value = accumulator };
```

This will parse and compute the value of a decimal number without requiring any
external functions.

Note, for the purpose of checking whether different rules have the same head,
the parameter list doesn't matter. That is,

```ipg
R(a, b, c) -> A;
R(x) -> A;
```

still conflict.

Also, in the case that a locally defined attribute uses the same name as a parameter,
it *does not* shadow it. That is an identifier in an expression will always refer to
the parameter even if there is an attribute with the same name. (Perhaps I will change
this in the future and/or make such shadowing entirely illegal.)

There is also no "staging" limitation on parameters. As the `Decimal` example illustrates,
you can absolutely have parameter values that are derived from the parsed data.

## Intervals

So far, everything above could be viewed as a more or less standard grammar formalism
with some "convenience" features. What makes IPG more geared to binary parsing is, as
the name suggests, the use of intervals. We've been implicitly using them the whole time,
and explicitly in `{ x = *[l] }`.

The core idea is when we invoke a rule, we also specify which interval it should be
restricted to. That interval will now be the whole input as far as that rule invocation
is concerned. After the invocation completes, the `START` and `END` attributes of (the
local copy of) the rule will be bound to the beginning and end of the interval actually
parsed. Within a rule, `EOI` refers to the offset of the end of the interval it was
invoked upon relative to its start. `0` is always the beginning of the slice of the
input the rule was invoked upon.

Let's see some examples.

If the string "hello" should occur at offset 5 of the input, we could match it via:

```ipg
S -> "hello"[5, 10];
```

Since terminals only need to match a *prefix* of their input, we could use:

```ipg
S -> "hello"[5, EOI];
```

to have the terminal consider the whole slice beginning at offset 5.

Empty intervals, i.e. intervals of the form `[l, l]`, are allowed. However,
invalid intervals, i.e. `[l, r]` where `l > r` or where `l < 0` or `r > EOI`,
cause the parse to fail even if they are applied to rules that don't consume
any input. This is a normal parse failure, not some kind of panic. It is totally
reasonable to make rules that will eventually lead to invalid intervals. For
example, if you wanted to parse 16 byte chunks until you ran out of input.

The defined `START` and `END` attributes allow us to have parsing follow *or precede*
the content actually parsed by a rule. For example, let's say at offset 5 we had
either the string "hello " followed by a name or "hi " followed by a name. We
could parse that with:

```ipg
S -> Salutation[5, EOI] Name[Salutation.END, EOI];

Salutation -> "hello " / "hi ";
```

In fact, this pattern of `R[Prev.END, EOI]` where `Prev` is the term preceding this
one is very common and is what would be inferred by the system. That means the above
`S` rule is equivalent to simply:

```ipg
S -> Salutation[5, EOI] Name;
```

As another bit of interval inference, if we only give one value in the interval, it
is interpreted as a length relative to the `END` of the previous term. So if we knew
the `Name` was at most 10 bytes, we could write either of the following equivalent rules:

```ipg
S1 -> Salutation[5, EOI] Name[Salutation.END, Salutation.END + 10];
S2 -> Salutation[5, EOI] Name[10];
```

With intervals fully specified and statically known, we could parse the terms in
any order. Of course, in the above example `Salutation.END` isn't statically known.
This means `Salutation` must be parsed before `Name`. Ultimately, as mentioned before
the order of executing the terms is determined by data dependencies. Before we introduced
intervals, every term (except attribute definitions) had a data dependency on the
prior term which led to a left-to-right order. With explicit intervals, the parsing can
happen in different orders. An at abstract example, the following two rules are equivalent:

```ipg
S1 -> A[0, B.START] B[0, EOI];
S2 -> B[0, EOI] A[0, B.START];
S3 -> B A[0, B.START];
```

All of these parse `B` first, and then parse `A` as preceding `B`. The `S1` version list
the non-terminals in the order they occur in the input which is arguably more natural.

`{ x = *[l, r] }` binds `x` to the slice of the input indicated by the interval `[l, r]`.
Interval inference applies, so, for example, if we wanted to allow a `Name` to be any
sequence of bytes, we could define it as `Name -> { name = * };`. This is shorthand for
`Name -> { name = *[0, EOI] };`. In our above example, where `Name` is invoked as
`Name[10]`, this would lead to `name` being bound to the whole input which would be the
10 byte slice. Of course, if we wanted a `Name` to be 10 bytes no matter how it was
invoked, we could define it as `Name -> { name = *[10] };`.

`{ x = . }` doesn't take a full interval since it always parses 1 byte. `{ x = .[l] }`
treats `l` as an offset, not a length.

Running `ipgcc` with `-t CORE` will output a pretty-printed representation of the
grammar after performing interval inference and rearranging the terms in data
dependency order.

### Type-Length-Value (TLV)

A common pattern in binary formats is the "type-length-value" pattern where there is
a field which specifies the type of a value followed by a length then followed by the
value. This is easily parsed as follows:

```ipg
TLV
  -> Type { type = Type.tag }
     Length { length = Length.value }
     Value(type)[length] { value = Value.value };
```
Written this way, even if you make a mistake in defining the `Value` parser, it will
never go outside of the slice you've given it. It will not impact the parsing of
following TLV entries.

To make the above example a bit more concrete, let's give plausible definitions to
`Type`, `Length`, and `Value`.

```ipg
Type -> { tag = . };

Length -> { lo = . } { hi = . } { value = (hi << 8) | lo };

Value(type)
  -> ?[ type == 1 ] Type1Value { value = Type1Value.this }
   / ?[ type == 2 ] Type2Value { value = Type2Value.this }
   / { value = * }; // Unknown, just store the bytes.
```

### Multiple Passes

There is absolutely nothing that stops different terms from being given the same or
overlapping intervals. This can be useful for structures that require multipass parsing.

### Repetition and Intervals

For something like `repeat A.x`, we usually want the individual instances of `A` to
follow each other in the input. That is, we want something like `repeat A[A.END, EOI].x`.
The problem here is: what should `A.END` be for the first iteration? To solve this
problem, the full form of `repeat A.x` is `repeat A[l, r].x starting on [l0, r0]`.
Now the first instance of `A` will be `A[l0, r0]` and the remaining will be `A[l, r]`.
The `[l0, r0]` interval gets populated via interval inference like everything else.
`repeat A.x starting on [l0, r0]` becomes `repeat A[A.END, EOI].x starting on [l0, r0]`, and
`repeat A[l].x starting on [l0, r0]` becomes `repeat A[A.END, A.END + l].x starting on [l0, r0]`.

The full form of `repeat A.x until B` is similarly `repeat A[l, r].x starting on [l0, r0] until B`.

These `repeat` forms are quite powerful. For example, in:

<small>[test/interpret/test-tutorial-7.ipg](test/interpret/test-tutorial-7.ipg)</small>
```ipg
Forward -> repeat A.x;
A -> "a" { x = "a" };

Backward -> repeat B[0, B.START].x;
B -> "z"[EOI - 1, EOI] { x = "z" };
```

`Forward` will parse 'a' characters from the beginning of the input until it hits a
byte that isn't 'a'. `Backward` on the other hand, will parse 'z' characters from the
end of the input until it hits the last byte that isn't 'z'.

As a more extreme example, the following grammar parses a linked list whose nodes
consist of 16-bit integers and links are 16-bit offsets. The nodes can be located
throughout the file in any order but the output will be in the order of the linked
list.

<small>[test/interpret/test-linked-list.ipg](test/interpret/test-linked-list.ipg)</small>
```ipg
U16 -> { lo = . } { hi = . } { value = (hi << 8) | lo };

Node -> U16 { value = U16.value} U16 { next = U16.value };

LinkedList -> repeat Node[Node.next, Node.next + 4].value starting on [0, 4];
```

`LinkedList` will terminate when the `Node.next` goes out of bounds. That could be
never on an input larger than 64KB or if the linked list is cyclic. You could easily
adapt the above to look for some other way of indicating the end of the linked list.
By maintaining a set of seen node using external functions, you could also detect
cyclic lists if that was a concern. The point of this example, though, was to show
the flexibility of the `repeat` construct when you can also control the interval.
(That said, if we used a packrat-style implementation where we cached the result
of the result of each rule for every interval, then the parser *would* terminate
on a cyclic list and would return a cyclic list. I've opted against implementing
packrat parsing as my guess is that most binary formats wouldn't benefit from it.)

### For-Loops

A common pattern in binary files is a "directory"-like pattern where one data structure
describes the location of other data structures. Typically, there is a count of the
number of entries somewhere. `for`-loops facilitate this case. The syntax is:

```ipg
for i = s to e do A[l, r]
```

where `s`, `e`, `l`, and `r` are all expressions and the `i` gets bound in `A[l, r]`.

This term allows iterating a known number of times over computed intervals. After this
term executes you can access the attributes of the iteration with the syntax `A(j).x`.
The indexing here is consistent with the indexing of the loop. For example, after
`for i = 5 to 10 do A[i, i+1]`, `A(5).x` is the `x` attribute of the *first* entry.
`A(1).x` would be an out-of-bounds access. You can also access all the results together
via `A.these`. This is a 0-based array, so in the previous example, we have
`A(5).this` and `A.these[0]` refer to the same thing.

A realistic example of using `for`-loops is parsing the sections of an ELF file. (You
can see the full grammar in `test/node/test-elf.ipg`.) Here a header is parsed by the
`H` rule which gives the offset and count (and size) of a series of section headers.
These are parsed in the first `for`-loop. The second `for`-loop uses the offsets of
the actual sections as provided by the section headers parsed in the first `for`-loop.
Notice how the sections could be anywhere in the file in any order. Indeed, they can
overlap with each other or the section headers.

```ipg
ELF
  -> H[0, 128]
     for i = 0 to H.e_shnum do SH[H.e_shoff + i*H.e_shentsize, H.e_shoff + (i + 1)*H.e_shentsize]
     for i = 1 to H.e_shnum do Sec(SH(i).sh_type)[SH(i).sh_offset, SH(i).sh_offset + SH(i).sh_size]
     ...;
```

`for i = s to e do A[l, r]` could almost be understood via translation into the invoking
`R(s, e)[0, EOI]` where R is the following parameterized rule (assuming `end` is not
free in `l` or `r`):

```ipg
R(i, end)
  -> ?[ i < end ] A[l, r] R(i + 1, end)[0, EOI] { these = cons(A.this, R.these) }
   / { these = nil() };       
```

The reason this translation doesn't work in general is that `l` and `r` can refer to
`A.START` and/or `A.END` which would refer to the wrong things in this translation.

### External Parsers

The JavaScript back-end generates rules as functions (and the same should be expected of
future back-ends though the exact details will likely be different).
For the JavaScript back-end, these functions take a string or
`Uint8Array` (or anything that looks sufficiently like one), and start and end offsets.
The functions output `null` on a parse failure, or an object containing the attributes
as fields plus the fields `_ipg_start` and `_ipg_end` that correspond to the amount of
data actually parsed, i.e. the are used to define the `START` and `END` attributes,
though these values are not offset to the start of the parser. Any parameters are passed
as additional arguments after the start and end offsets.

Any function meeting this interface can be used as a parser. To avoid error reports,
such external parsers need to be declared. This can be done with a line starting
with `%declare` in the grammar section of the file (i.e. not in the pre- or post-ambles).
This takes a whitespace-separated list of names terminated by `%end` (which doesn't
need to be at the start of the line). All this declaration does is suppress validation
errors. It changes nothing about the output.

In the likely case that your external parser can be expected to (logically) consume
all its input, then you can use the following wrapper.

```javascript
function MyParser(input, start, end, ...args) {
    const result = actualParser(input.slice(start, end), ...args);
    if (result === null) return null;
    return { _ipg_start: 0, _ipg_end: start - end, ...result };
}
```

where `actualParser` is the actual parser that expects some buffer to work on
and returns `null` on parse failure and an object on parse success. You would
add `%declare MyParser %end` to the file to declare this. Obviously,
this can be adapted to the actual interface of your parser.

### Absolute Offsets, or the Lack Thereof

There is no mechanism to specify an absolute interval. All intervals are
relative to the slice of the input they've been given. This makes the parsers
easier to analyze and more modular in the sense that they can only depend on
the input they've been given. More specifically, they *can't* care about *where*
that input is located in some broader "file".

The downside is that absolute offsets are not uncommon in binary formats. The
"principled" solution is to percolate these absolute offsets up to some logical
"root" level where the relative offsets would correspond to the (logical) "absolute"
offsets. (They still wouldn't be truly absolute offsets, but that is a good thing.
If I embed an ELF file into a container format, the "absolute" offsets in the ELF
file are relative to the beginning of the ELF file, not the beginning of the container.)

The grammar for ELF (and just the excerpt included above) illustrates this pattern.
We get the "absolute" offsets to the sections from the section headers and return
those to the top-level rule which then uses them to parse the sections.

One could imagine a different organization of the parser where each section is
parsed immediately following the parsing of the corresponding section header entry.
This would be awkward to do in IPG. To do this the "actual" starting offset of
the section header could be passed down as a parameter while the section header
parser is given the full interval. `DirectoriesRecursive` in the ISO9660 parser
illustrates this pattern. The top-level parser invokes it with the full interval,
i.e. `[0, EOI]`, and it passes that interval down into its recursive calls.

<small>[test/node/test-iso9660.ipg](test/node/test-iso9660.ipg)</small>
```ipg
DirectoriesRecursive(logicalBlockSize, node)
    -> { offset = logicalBlockSize * get(node, "locationOfExtent") }
       { record = node }
       DirectoryRecords[offset, offset + get(node, "dataLength")]
       for i = 2 to length(DirectoryRecords.values) do // First two records are '.' and '..'.
         DirectoriesRecursive(logicalBlockSize, DirectoryRecords.values[i])[0, EOI]
       { children = DirectoriesRecursive.these };

DirectoryRecords -> repeat DirectoryRecord.this;
```

While it wouldn't be hard to add some mechanism to have absolute intervals, I
don't *think* this is that significant a limitation in practice, and the benefits
of *not* having absolute offsets seem pretty nice. While I "work around" the lack
of absolute offsets in the excerpt above, ISO9660 provides other mechanisms for
listing the files that doesn't require this recursive traversal. This is also
illustrated in the full ISO9660 grammar.
