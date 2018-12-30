module Time = {
    /**
     * The goal of this module is to create a type that can
     * be used as a key on an OrderedMap. Thus simply adding
     * a value to the map would place it in chronological order
     */
    type t = {
        year: int,
        month: int,
        day: int,
        hour: int,
        minute: int,
    };

    /* Convert type t into a timestamp using js date, necessary for accurate compare */
    let getStamp = (time) => {
        let { year, month, day, hour, minute } = time;
        let x = Js.Date.makeWithYMDHM(
            ~year=float_of_int(year),
            ~month=float_of_int(month - 1),
            ~date=float_of_int(day),
            ~hours=float_of_int(hour),
            ~minutes=float_of_int(minute),
            ()
        );
        Js.Date.valueOf(x);
    };

    /* compare timestamps to determine order and equality */
    let compare = (first, second) => {
        let firstMins = getStamp(first);
        let secondMins = getStamp(second);
        switch(firstMins >= secondMins) {
            | true when firstMins == secondMins => 0
            | true => 1
            | false => -1
        }
    };
}

module Guard = {
    /**
     * The Guard Module is used to keep track of
     * Guards and the number of minutes they have
     * slept in a frequency map. Every minute the
     * guard is asleep is tracked on a punch card
     * incrementing the frequency each time a unique
     * minute is seen.
     */
    type id = int;
    type t = {
        id,
        sleptMinutes: Belt.Map.Int.t(int)
    };

    /* create a new guard with a blank frequency map */
    let make = (~id: int) => {
        id,
        sleptMinutes: Belt.Map.Int.empty,
    };

    /* not used, keeping for posteriy */
    let compare = (a, b) => {
        switch(a.id >= b.id) {
            | true when a.id == b.id => 0
            | true => 1
            | false => -1
        }
    };

    /**
     * Add a new nap to the guard record. Iterates over
     * the new nap's minutes and checks the guard's freqmap
     * for that minute. if a value is found, increment it,
     * otherwise set it to 1 for its first occurrence 
     */
    let trackNap = (guard: t, nap: array(int)) => {
        let sleptMinutes = Array.fold_left((map, napMinute) => {
            map -> Belt.Map.Int.update(napMinute, v => switch(v) {
                | Some(previous) => Some(previous + 1)
                | None => Some(1)
            })
        }, guard.sleptMinutes, nap);
        {
            ...guard,
            sleptMinutes
        }
    };
}

/* There are three log actions we care about: ShiftChanges, Waking Up and Going to sleep */
type action = ShiftChange(Guard.id) | WakeUp | FallAsleep;

module Nap = {
    /**
     * Could be a little too light to be its own Module,
     * but simply is an opaque type of array(int) with a 
     * custom make function that creates an array with the
     * minutes between two Time.t's
     */
    type t = array(int);

    /**
     * Create a new array of length of b.minute - a.minute,
     * initialized with incrementing values starting at a.minute,
     * and ending at b.minute - 1. Guards count as being asleep
     * the minute they fall asleep (a) and awake the minute they
     * wake up (b)
     */
    let make = (a: Time.t, b: Time.t): t => {
        Array.init(b.minute - a.minute, i => a.minute + i)
    };
}

/* Create an OrderedMap type that takes Time.t as keys */
module TimeCard = Map.Make(Time);

let datetimeRegex = Js.Re.fromString("\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]");
let shiftChangeRegex = Js.Re.fromString("Guard #(\d+)");
let fallsAsleepRegex = Js.Re.fromString("falls asleep");

let getRawEntryType = (str) => {
    switch(Js.String.match(shiftChangeRegex, str)) {
        | Some(matches) => ShiftChange(int_of_string(matches[1]))
        | None => {
            if (Js.Re.test(str, fallsAsleepRegex)) {
                FallAsleep
            } else {
                WakeUp
            }
        }
    }
};

let matchesToInt = (result) => {
    switch(result) {
        | Some(matches) => Some(Array.sub(matches, 1, 5) |> Array.map(int_of_string))
        | None => None
    }
}

let parseRawEntry = (str) => {
    let date: Time.t = switch(Js.String.match(datetimeRegex, str) -> matchesToInt) {
        | Some(matches) => {
            year: matches[0],
            month: matches[1],
            day: matches[2],
            hour: matches[3],
            minute: matches[4],
        }
        | None => {
            year: 0,
            month: 0,
            day: 0,
            hour: 0,
            minute: 0,
        }
    };

    (date, getRawEntryType(str))
};

let timeCard = Utilities.fileToListByNewLines(~dir=[%bs.node __dirname], "input.txt") |> List.fold_left((map, str) => {
    let (date, action) = parseRawEntry(str);
    map |> TimeCard.add(date, action);
}, TimeCard.empty);

let buildGuardProfiles = (log) => {
    let guardTable = Belt.MutableMap.Int.make();
    let rec processTick = (previousTick, currentGuard: Guard.t, remainingTicks) => {
        switch(remainingTicks -> TimeCard.min_binding) {
            | nextTick => {
                let (nextDate, nextAction) = nextTick;
                let (previousDate, previousAction) = previousTick;
                let guard = switch(previousAction) {
                    | FallAsleep => {
                        switch(nextAction) {
                            | WakeUp => {
                                let nap = Nap.make(previousDate, nextDate);
                                currentGuard -> Guard.trackNap(nap);
                            }
                            | _ => currentGuard
                        }
                    }
                    | _ => {
                        switch(nextAction) {
                            | ShiftChange(id) => {
                                switch(guardTable -> Belt.MutableMap.Int.get(id)) {
                                    | Some(guard) => guard
                                    | None => Guard.make(~id)
                                }
                            }
                            | _ => currentGuard
                        }
                    }
                };
                guardTable -> Belt.MutableMap.Int.set(guard.id, guard);
                processTick(nextTick, guard, TimeCard.remove(nextDate, remainingTicks));
            }
            | exception Not_found => guardTable
        }
    }
    let firstTick = log -> TimeCard.min_binding;
    let (firstDate, firstAction) = firstTick;
    let initialGuard = switch(firstAction) {
        | ShiftChange(id) => Guard.make(~id)
        | _ => Guard.make(~id=0)
    };
    processTick(firstTick, initialGuard, TimeCard.remove(firstDate, log));
}

let guards = buildGuardProfiles(timeCard);
let tabulateTotalMinutes = (sleptMinutes: Belt.Map.Int.t(int)) => {
    sleptMinutes -> Belt.Map.Int.reduce(0, (current, k, v) => current + v);
}

let getMostAsleepGuard = (guards: Belt.MutableMap.Int.t(Guard.t)) => {
    Belt.MutableMap.Int.reduce(guards, None, (current, id, guard) => {
        switch(current) {
            | None => Some(guard)
            | Some(prevGuard) => {
                let totalPrevMinutes = tabulateTotalMinutes(prevGuard.sleptMinutes);
                let totalMinutes = tabulateTotalMinutes(guard.sleptMinutes);
                if (totalMinutes > totalPrevMinutes) {
                    Some(guard);
                } else {
                    Some(prevGuard);
                }
            }
        }
    });
}

let getMostAsleepMinute = (guard: Guard.t) => {
    Belt.Map.Int.reduce(guard.sleptMinutes, None, (current, k, v) => {
        switch(current) {
            | None => Some((k, v))
            | Some((pk, pv)) => {
                if (v > pv) {
                    Some((k, v))
                } else {
                    Some((pk, pv))
                }
            }
        }
    })
};

let theTarget = switch(getMostAsleepGuard(guards)) {
    | Some(guard) => guard
    | None => raise(Not_found)
};

let theMin = switch(getMostAsleepMinute(theTarget)) {
    | Some((m, _)) => m
    | None => raise(Not_found);
}
Js.log3(theTarget.id, theMin, theTarget.id * theMin);