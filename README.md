# legendre-symbol-calculator

This was a fun little app I put together while revising for a number theory
exam. The core idea is that the app helps you teach yourself to determine
whether a given number *y* is a *quadratic residue* modulo some odd prime *p*,
that is, if there exists *x* such that *x^2* is congruent to *y* mod *p*.

The Legendre symbol *(y/p)* is defined to be 1 if *y* is a quadratic residue
modulo *p*, and -1 otherwise.

The phraes "the notes" within the evaluation rules refers to the set of lecture
notes which were used in the 2017/18 Introduction to Number Theory course at
the University of Edinburgh. If you don't have those notes, a little googling
will hopefully help you find proofs of these facts; alternatively, you might
try to prove them yourself!

## Deployment

Simply run `pulp build --to dist/app.js` and then stick `app.js` and
`index.html` in the same directory on a web server.
