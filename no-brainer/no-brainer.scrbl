#lang scribble/doc

@(require scribble/manual)

@title{The "No-Brainer" Tool}

The No-Brainer Tool performs a static check for arity errors 
and unused bindings on the program in the definitions window.

@section{Arity Errors}

No-Brainer operates on any sequence of top-level definitions, but
makes a weak soundness claim ONLY for code contained in top-level
module expressions.

No-Brainer makes the following soundness-like claim: if No-Brainer
identifies a piece of code as containing an arity error, then that code (if
evaluated) will raise an exception[*].

No-Brainer makes no specific completeness-like claim. No-Brainer makes two
passes; the first to associate identifiers with arity specifications, the
second to identify arity errors.  No-Brainer will correctly associate lambda
expressions with definitions (both lexical and top-level) when the
@scheme[lambda] expression is directly contained by the definition or is the
only possible evaluation result of the expression directly contained in the
definition.  Any mutation of the identifier causes No-Brainer to remove this
identifier from its table (and hence to make no claims about its applications).
No-Brainer will identify arity errors only in applications where the function
position is directly an identifier.

@section{Unused Bindings}

No-Brainer also reports unused bindings in the user's program. In particular,
No-Brainer reports an error for each @scheme[let], @scheme[letrec], or
@scheme[lambda] which meets the following criteria:

@itemize{
@item{It binds an identifier which is not referred to anywhere in
that identifier's scope.}
@item{The unused identifier has a source position.}}

The second of these is intended to filter out bindings introduced by macro
expansion, as these are less likely to be errors in the user's program.  Note
that the position check is applied to the identifier, and not to the
@scheme[let], @scheme[letrec], or @scheme[lambda], so that macro-based special
forms which bind identifiers specified by the user will be treated like
ordinary binding forms.

Since unused bindings are less likely to be errors than arity 
mismatches, they are reported in a later section of the output.  
Also, since unused lambda bindings are less likely to be errors 
than unused let/rec bindings, the unused bindings error messages
are separated into these two categories

@section{Installing the Tool}

No-Brainer is a Planet package.  It's one of those planet packages that adds a
tool rather than providing functions, though, so there's a stub file just so
that you can install it using a planet require.

Evaluate this program:

@schememod[
scheme
(require (planet clements/no-brainer))
]

... in the module language, and then restart DrScheme to see the tool's button.

@section{Using the Tool}

To use No-Brainer, click the @onscreen{No Brain} button.  No-Brainer
will print a series of errors detected.  All output is confined to a
window entitled @onscreen{Things you might want to Fix.}

The user's interface to no-brainer is primitive, in that references to
source-code positions are accomplished by mzscheme's native format
ability to represent syntax objects.  That is, a syntax object which
refers to line and line position within the current file will be 
printed so as to reveal that information.

Example:

@verbatim[#<<|
...
bindings (end-selection) (from (#<syntax:2487:22>)) unused in expression #<syntax:2484:14>
...
|
]

@section{Running Time}

No-Brainer makes two passes over the code, and visits each expression once 
during each pass. In theory, it could be quadratic in running time, as it must
perform a set-union operation on lists of identifiers at each step, but this
should never dominate the running time.  No-Brainer is observed to take 
roughly ten seconds on files of roughly two thousand lines.

Useful? You decide.

[*] Note that the exception may not be an arity error, but instead 
an undefined/uninitialized error. This can happen because the application
precedes the definition, or because the evaluation of the definition
raised an exception.

