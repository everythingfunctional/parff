parff
=====

[![pipeline status](https://gitlab.com/everythingfunctional/parff/badges/main/pipeline.svg)](https://gitlab.com/everythingfunctional/parff/commits/main)

Parser for Fortran. The foundations of a functional style parser combinator library.

By using a library like this, it is possible to implement parsers for complex
formats by composing independent little pieces. A good example of a project that
has made successful use of this library is
[jsonff](https://gitlab.com/everythingfunctional/jsonff).

## Tutorial

A frequent requirement in software is the parsing of some textual data
into the internal representation required by our program.
This is often more difficult than it may at first appear.
It usually results in some hand-rolled, state machine base implementation,
specifically coded for the custom format we're using.
And the difficulty in writing some reusable parsing library is that
it cannot know ahead of time all the data types and formats that
may need to be parsed.

However, this is a problem that has (I think) finally been quite satisfactorily solved by parser combinator libraries.
The most user friendly and well known examples come from functional programming languages.
There are great tutorials for those available, for example
[this one using F#](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/),
and [this one for Haskell's parsec](https://hasura.io/blog/parser-combinators-walkthrough/).

These implementations make use of features of those languages that Fortran does not have,
i.e. lambda functions, closures, currying, and type inference/unification.
This makes these libraries easier to implement and more convenient to use,
but do not make it impossible to implement something similar in Fortran, as we shall now demonstrate.

### What is a Parser?

At it's most basic, a parser is a function that takes some string,
and based on its contents, produces a value.
In Fortran, that would look something like the following:

```Fortran
abstract interface
  function parser_i(string) result(output)
    character(len=*), intent(in) :: string
    ??? :: output
  end function
end interface
```

As you can see, we don't yet know what the type of the output should be.
There is another wrinkle as well.
What if the string doesn't contain a valid representation of the type of output we'd like to produce?
We'd like the function to produce either an output or an error.
Type inference, pattern matching and higher kinded types allow functional languages to solve this problem rather easily and conveniently,
but this is something we can address in Fortran, albeit in a slightly more clunky way,
by having our function return something like the following:

```Fortran
type :: parser_output_t
  type(error) :: error
  class(*) :: output
  logical :: ok ! was parsed correctly
end type
```

An example of what that might look like is as follows.
Suppose we would like to parse the letter "A".
Such a parser might look like:

```Fortran
function parse_a(string) result(output)
  character(len=*), intent(in) :: string
  type(parser_output_t) :: output

  if (string == "A") then
    output%ok = .true.
    output%output = "A"
  else
    output%ok = .false.
    output%error = "Expected an A" ! Assuming this works for our `error` type
  end if
end function
```

### What is a Combinator?

So why should I bother conforming to that interface? What does this buy me?
Let's say you also have a function `parse_b`,
but now I need to parse an "A" and then a "B".
Should we write a new function `parse_a_then_b`?
Why can't I just use the two parsers I already have?
That's where combinators come in.

But first, we need to address some additional complexity in our interface.
Partly this comes in the interest of being able to provide more meaningful error messages.
If parser fails, we'd like to be able to say where in the string the problem occurred.
We can enable this by passing our current position along to the parser.
We'll package our string and position into a derived type like the following for convenience.

```Fortran
type :: state_t
  type(varying_string) :: input ! we'll start using this instead of raw characters
  type(position_t) :: position ! the internals of this aren't particularly important
end type
```

Now, we're probably going to want to put "A" and "B" together as the output from our combined parser.
This is such a common pattern, where one parser needs access to the output of the parser before it,
that we'll need to accommodate this as part of our library of combinators.
Thus, we'll also have the following interface.

```Fortran
abstract interface
  function then_parser_i(previous, state_) result(result_)
    class(parsed_value_t), intent(in) :: previous
    type(state_t), intent(in) :: state_
    type(parser_output_t) :: result_
  end function
end interface
```

At this point, we do still need to write a new function, `then_parse_b`,
but it can call `parse_b` rather than re-implement it's logic,
and can be used after parsers other than `parse_a`.
It looks something like the following.

```Fortran
function then_parse_b(previous, state_) result(result_)
  class(parsed_value_t), intent(in) :: previous
  type(state_t), intent(in) :: state_
  type(parser_output_t) :: result_

  result_ = parse_b(state_)
  if (result_%ok) then
    select type (previous)
    type is (parsed_character_t)
      select type (b => result_%parsed)
      type is (parsed_character_t) ! unfortunately these select types are necessary
        result_ = result_%with_parsed_value(parsed_string_t(previous%char // b%char))
      end select
    end select
  end if
end function
```

Given this, we now can now implement our `parse_a_then_b` (if we'd actually like to have it as it's own function)
with a single line, `result_ = sequence(parse_a, then_parse_b, state)`.

The `sequence` function is what is called a combinator, it combines two parsers,
and has an implementation like the following.

```Fortran
function sequence(parser1, parser2, state) result(result_)
  procedure(parser_i) :: parser1
  procedure(then_parser_i) :: parser2
  type(state_t), intent(in) :: state
  type(parser_output_t) :: result_

  result_ = parser1(state)
  if (result_%ok) then
    result_ = parser2(result_%parsed, result_%new_state)
  end if
end function
```

### Simple Parsers

(effectively) Every parser is implemented in terms of the basic parser, `satisfy`.
`satisfy` has the following interface, with the basic logic of;
if the first character in the given string produces true with the given logical function then the output is the first character,
otherwise the result is an error stating that it found the first character.

```Fortran
function satisfy(matches, state) result(result_)
  procedure(match_i) :: matches
  type(state_t), intent(in) :: state
  type(parser_output_t) :: result_
end function

function match_i(char_) result(matches)
  character(len=1), intent(in) :: char_
  logical :: matches
end function
```

This is where the unfortunate clumsiness of this approach in Fortran presents itself.
You'll notice that the `satisfy` function does not conform to either the `parser_i` or `then_parser_i` interfaces,
and thus is not able to be used as one of the arguments to any of the combinators.
In fact, this is a problem for most of the simple parsers, as they generally require an additional parameter.
Other languages can get around this problem by using currying
(a function that is not passed enough arguments becomes a function that takes only the remaining arguments),
or by using closures, where a function returns another function that can refer to the original argument.
Unfortunately, neither of these techniques are available in Fortran.
This means that many of the primitive/simple parsers must be wrapped in intermediate functions.
In practice, while this adds to the verbosity, it is rarely too much of a burden.

Other simple parsers provided are:

* `parse_nothing` - consumes none of the string, and returns a `NOTHING` value
* `parse_end_of_input` - returns a `NOTHING` value, but only if there is no string left to parse
* `parse_char` - parse only the given character
* `parse_string` - parse exactly the given string
* `parse_digit` - returns the next character if it is one of `0123456789`
* `parse_integer`/`parse_rational` - returns an `integer`/`double precision` value, consumes as much of the string as can be parsed (at least 1 character)
* `parse_whitespace` - returns the next character if it is space, tab, newline or carriage return

### Combinators

We've already seen one combinator, the `sequence` function, but there are a variety of them.

One that is frequently used, and is essential for providing more helpful error messages, is the `with_label` function.
It doesn't combine parsers, but rather, if the given parser fails, gives a meaningful statement about what was expected.
Notice that the `satisfy` parser does not know what was expected, and thus cannot say.

The `sequence` combinator allows combining the results of running one parser after another.
The `drop_then` and `then_drop` also allow running one parser after another, but discarding one of the results.
`return_` uses the given value as the output without looking at the input.

`optionally` will always succeed, returning nothing if the given parser failed.
`either` will attempt to use the second parser if the first one fails.

The following parsers allow using a parser multiple times:

* `many` - return an array of results, 0 or more, as long as the parser continues to succeed
* `many1` - same as `many`, but fails if 0 results are produced
* `many_with_separator` - same as `many`, but uses the second parser between each use of the first
* `many1_with_separator` - same as `many_with_separator`, but fails if 0 results are produced
* `repeat_` - return an array of results, running the parser exactly the given number of times
