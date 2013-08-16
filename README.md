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