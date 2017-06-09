# wrapper

A demo wrapper for HACKER, the stacking block solver.
HACKER was originally written by Gerald Sussman for his PhD.  It was
re-written in Clojure by Dylan Holmes.
WRAPPER was written by Ben Wen to run the code with Leiningen tooling.

## Installation

Download from https://github.com/benzenwen/wrapper

Requires: [Leiningen 2.7+](https://leiningen.org),
[Clojure 1.8.0+](https://clojure.org), and [Java 1.6, 1.7, 1.8](https://java.com/download)

## Usage

```
git clone https://github.com/benzenwen/wrapper.git
cd wrapper
lein repl
(run-demo-1)
```

## Examples

A successful example

```
=> (run-demo-1)
(make (on A TABLE))
(make (on B TABLE))
(make (on A B))
{:scene {:facts ((on A B 0) (on B TABLE 1)), :blocks {TABLE {:width Infinity, :name TABLE}, A {:width 1, :name A}, B {:width 1, :name B}}}, :callstack []}
```

An unsolvable example
```
=> (run-demo-2)
(make (on B TABLE))
(make (on C TABLE))
(make (on A B))
(make (on B C))
error [!unsatisfied-prerequisite [not [on A B]]]
patching error:  [!prerequisite-missing make-on :line1 (cleared-top B)]
error [!failed-conditional (assign (:y) (on :y B))]
		(make (not (on A B)))
		(make (exists (:z) (not (= :z B)) (on A :z)))
			(make (on A C))
error [!fatal-error ERROR_NO_SPACE_ON_BLOCK]
{:scene {:facts ((on A C 0) (on C TABLE 1) (on B TABLE 0)), :blocks
... :error [!fatal-error ERROR_NO_SPACE_ON_BLOCK]}
```

See the Clojure (HACKER
documentation)[http://logical.ai/auai/doc/doc-hacker.html] page for
more examples.

### Documentation

(HACKER)[http://logical.ai/auai/doc/doc-hacker.html]

### To Do

1. Wrap with a web interface.
2. More docs.
3. More examples.
4. Make -main do something interesting.

### Bugs

None known, yet.


## License

Copyright © 2017 Benson Wen
@benzenwen

Code distributed under the GNU Public License 2.0+.  Documentation and
other artifacts distributed under Creative Commons Attribution-Share
Alike 4.0 International License, to the extent allowed.

Much thanks to Dylan Holmes, Gerry Sussman.  Thanks to Durant Schoon
for suggesting the project.
