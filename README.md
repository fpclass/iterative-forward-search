# Iterative Forward Search

![MIT](https://img.shields.io/github/license/fpclass/iterative-forward-search)
[![CI](https://github.com/fpclass/iterative-forward-search/actions/workflows/haskell.yaml/badge.svg)](https://github.com/fpclass/iterative-forward-search/actions/workflows/haskell.yaml)
[![stackage-nightly](https://github.com/fpclass/iterative-forward-search/actions/workflows/stackage-nightly.yaml/badge.svg)](https://github.com/fpclass/iterative-forward-search/actions/workflows/stackage-nightly.yaml)
[![iterative-forward-search](https://img.shields.io/hackage/v/iterative-forward-search)](https://hackage.haskell.org/package/iterative-forward-search)

This library implements a contraint solver via the [iterative forward search algorithm](https://muller.unitime.org/lscs04.pdf). It also includes a helper module specifically for using the algorithm to timetable events.

## Usage

To use the CSP solver first create a `CSP` value which describes your CSP, for example
```haskell
csp :: CSP Solution
csp = MkCSP {
    cspVariables = IS.fromList [1,2,3],
    cspDomains = IM.fromList [(1, [1, 2, 3]), (2, [1, 2, 4]), (3, [4, 5, 6])],
    cspConstraints = [ (IS.fromList [1, 2], \a -> IM.lookup 1 a != IM.lookup 2 a)
                     , (IS.fromList [2, 3], \a -> IM.lookup 2 a >= IM.lookup 3 a)
                     ],
    cspRandomCap = 30, -- 10 * (# of variables) is a reasonable default
    cspTermination = defaultTermination
}
```

This example represents a CSP with 3 variables, `1`, `2` and `3`, where variable `1` has domain `[1, 2, 3]`, variable `2` has domain `[1, 2, 4]`, and variable `3` has domain `[4, 5, 6]`. The contraints are that variable `1` is not equal to variable `2`, and variable `2` is at least as big as variable `3`. It uses the default termination condition, and performs 30 iterations before we select variables randomly.

You can then find a solution simply by evaluating `ifs csp`, which will perform iterations till the given termination function returns a `Just` value.

### Timetabling

The `toCSP` function in `Data.IFS.Timetable` takes a mapping from slot IDs to intervals, a hashmap of event IDs to the person IDs involved, and a map of person IDs to the slots where they are unavailable and generates a CSP which can then be solved with `ifs`. For example:

```haskell
slotMap :: IntMap (Interval UTCTime)
slotMap = IM.fromList [(1, eventTime1), (2, eventTime2), (3, eventTime3)]

events :: HashMap Int [person]
events = HM.fromList [(1, [user1, user2]), (2, [user1])]

unavailability :: HashMap person (Set Int)
unavailability = HM.fromList [(user1, S.empty), (user2, S.fromList [1,3])]

csp :: CSP r
csp = toCSP slotMap events unavailability defaultTermination
```

This will generate a CSP that creates a mapping from the events 1 and 2 to the time slots 1, 2 and 3. 

## Limitations

- Variables and values must be integers
- Only hard constraints are supported
