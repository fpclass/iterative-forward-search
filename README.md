# IFS

![MIT](https://img.shields.io/github/license/fpclass/ifs)
[![CI](https://github.com/fpclass/ifs/actions/workflows/haskell.yaml/badge.svg)](https://github.com/fpclass/ifs/actions/workflows/haskell.yaml)
[![stackage-nightly](https://github.com/fpclass/ifs/actions/workflows/stackage-nightly.yaml/badge.svg)](https://github.com/fpclass/ifs/actions/workflows/stackage-nightly.yaml)
[![ifs](https://img.shields.io/hackage/v/ifs)](https://hackage.haskell.org/package/ifs)

This library implements a contraint solver via the [iterative forward search algorithm](https://muller.unitime.org/lscs04.pdf). It also includes a helper module specifically using the algorithm to timetable events.

## Usage

Placeholder

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

This will generate a CSP that creates a mapping from time slots 1, 2 and 3 to the events 1 and 2

## Limitations

- Variables and values must be integers
- Only hard constraints are supported
