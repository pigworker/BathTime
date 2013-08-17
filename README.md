BathTime
========

being an experiment with universes and time


Introduction
------------

I often write type checkers as a kind of reassuring exercise workout,
and this one is probably no different, but it has a few interesting
aspects. It's a *bidirectional* type checker for &beta;-normal forms in
a dependent type theory with a universe hierarchy. Canonical
constructions are always understood relative to a required type;
usages (i.e., variables and elimination forms) have synthesized types.
A *structural* subtyping check happens just when a synthesized type
must be good for a required type, and that's where we build in
*cumulativity*, allowing smaller universes to embed in larger
universes.

I write type checking judgments `ty :w> tm` and type
synthesis judgments `ne <w: ty`, where `ne` is the language of *neutral*
terms, embedded in `tm`, and `ty` is a suggestive synonym for `tm`.
By the way, the `w` is a *world*, about which more later.


A Universe Hierarchy
--------------------

We have universes `Set^n` for natural numbers, `n` (with `Set` short
for `Set^0`. Above the `Set` levels, we have `Type`, and further above
is the topsort, `Kind`. Any type which can be checked or synthesized
either inhabits `Kind` or is `Kind` itself. The typing rule for sorts
is just

      s' > s
    ------------
      s' :*> s

Checking types makes it easy to explain the sizes involved in compound types.
Function types, for example, exist at every level.

      s :*> S    x :w S |- s :*> T
    --------------------------------
      s :*> (x :w S) -> T

Note, in particular, that `Kind` admits `(x1 : S1) -> ... (xn : Sn) ->
Type`, so we can express types for *large* eliminators, rather than
requiring eliminators to have polymorphic motives. Of course, it's
easy to exhaust even this little extra headroom, but let's see how
things work out in practice.


Worlds/Phases/Stages/Times
--------------------------

I'm experimenting with a separate stratification of type theory into
what we usually call *worlds*, in a Kripke semantics. Typing happens
in a world. The variable rule enforces *accessibility* from the world
where a variable is bound to the world where it is used.

      x :w S    w |> u
    --------------------
      x <u: S

At present, I'm hardwiring two worlds, the `Dyn`amic (whose world
annotation is empty), for things which may exist at closed-run time,
and the `Sta`tic (whose world annotation is `*`), for things which
must exist only during typechecking. We have `Dyn |> Sta`. Please
don't

 * conflate the dynamic/static distinction with the set-theoretic small/large distinction (for we may have sets which talk of small data, and data large enough to represent sets);
 * imagine that because we typecheck before we run, the world accessibility relation should allow static-to-dynamic (we stop the dynamic world and typecheck its past in order to make predictions about its future, e.g. that if we stop it later, we shall have nothing to unlearn).

I'm expecting that the world structure will become more diverse (and
more customizable) as time goes by. Moreover, I expect that we shall
have constructions of the form "if this thing's pieces can be
constructed piecewise in a hierarchy of worlds, then it can be
constructed entirely in their limit". Even with two worlds, we'll see
a difference between product-like dynamic quantification `(x : S) ->
T` and intersection-like static quantification `(x :* S) -> T`. I'm
hoping for a major outbreak of sanity.


Some Religion and its Syntax
----------------------------

At least as far as run time is concerned, I'm avoiding new generative forms
of data. We have functions (represented as closures), and we have first order
data in three term forms

    ()                           -- nil
    term , term                  -- cons
    [list]                       -- isorecursive packing

where `,` associates rightward, and `list` syntax just sugars the same
notion of term, allowing

    , term                       -- meaning  term
    term list                    -- meaning  term , list
                                 -- meaning  ()

For example, `[a b c]` is short for `[, a, b, c, ()]`.

It should be possible to erase isorecursive packing at closed-run time, as
its purpose is only to show the typechecker where to unfold fixpoints. Otherwise,
data are from LISP. Of course you can't infer types for this stuff. The point is
to project types onto it. In time, we might play similar games with data yet
more raw. Fritz Henglein likes to observe that for some CS folk "it's all bits"
and for others "it's all structure". Both are right.

By the way, I allow `#n` for the list of `n` copies of `()`. Desugaring, we get

    #0 = ()
    #1 = () , ()
    #2 = () , () , ()

and so on. Rather than a unary encoding, I could choose one of the
many amusing isos between the natural numbers and the binary trees,
but I won't just now. It is my habit to display the head of an iso-packed
list in this form if possible, as that will often correspond to a choice
of constructor. So, a node of inductive data will tend to look like

    [#n blah blah blah , proof]

where `proof` will often be `()` and hence suppressed.

(There are lots of choices in this design space. Here, I'm using nil and cons
to capture both *tupling* and *distinction*. We might well find cause to separate
these two notions, especially as tupling can be lazy but distinction must be
strict. It may also be possible to make iso-packing a smart constructor, packing
only neutrals and other packings, but evaporating when given concrete contents.)


Specifying a Theory
-------------------

I'm trying to build some tools which enable me to write down the rules of
the theory in a compact and readable form, then generate all the equipment.
There'll be some interpretive overhead, but I'll take the hit to be lighter
of foot.