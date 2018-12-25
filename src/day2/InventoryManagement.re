
let inventoryList = Utilities.fileToListByNewLines(~dir=[%bs.node __dirname], "input.txt");

type matches = 
    | NoMatch
    | ExactlyTwice
    | ExactlyThrice
    | BothTwiceAndThrice;

let listHasMatchesWithLength = (len, list) => {
    (List.filter(k => List.length(k) == len, list) |> List.length) > 0;
}

let countMatchingCharacters = (stringA, stringB) => {
    let matching = ref(0);
    String.iteri((index, value) => {
        if (value == stringB.[index]) {
            matching := matching^ + 1;
        }
    }, stringA);
    matching^;
}

let getCommonChars = (stringA, stringB) => {
    let listA = Utilities.explode(stringA);
    let listB = Utilities.explode(stringB);
    List.fold_left2((a, b, c) => {
        if (b === c) {
            a ++ String.make(1, b);
        } else {
            a
        }
    }, "", listA, listB);
}

type searchResult = NotFound | Found(string, string);

let findCorrectSkus = (list) => {
    let initial = List.hd(list);
    let target = String.length(initial) - 1;
    let rec search = (stringA) => fun
        | [] => NotFound
        | [hd] => {
            switch (countMatchingCharacters(stringA, hd) == target) {
                | true => Found(stringA, hd)
                | false => NotFound
            }
        }
        | [hd, ...tl] => {
            switch (countMatchingCharacters(stringA, hd) == target) {
                | true => Found(stringA, hd)
                | false => search(stringA, tl)
            }
        }

    let matchFound = ref(false);
    let currentIndex = ref(1);
    let matchingChars = ref("");
    while(! matchFound^) {
        switch(search(List.nth(list, currentIndex^), list)) {
            | Found(a, b) => {
                matchFound := true;
                matchingChars := getCommonChars(a, b);
            }
            | NotFound => ()
        }
        currentIndex := currentIndex^ + 1;
    }
    matchingChars^
};

let findDuplicateChars = inventoryLabel => {
    let rec aux = (current, acc) => fun
        /* if the original list is empty we're done */
        | [] => []
        | [x] => [[x, ...current], ...acc]
        | [a, ...[b, ..._] as t] => {
            if (a == b) {
                aux([a, ...current], acc, t);
            } else {
                aux([], [[a, ...current], ...acc], t);
            }
        };
    let list = Utilities.explode(inventoryLabel) |> List.sort(Utilities.alphaSort);
    let matches = List.rev(aux([], [], list)) |> List.filter(k => List.length(k) >= 2);
    if (List.length(matches) === 0) {
        NoMatch
    } else {
        let exactlyTwo = listHasMatchesWithLength(2, matches);
        let exactlyThree = listHasMatchesWithLength(3, matches);
        switch (exactlyTwo, exactlyThree) {
            | (true, true) => BothTwiceAndThrice
            | (true, false) => ExactlyTwice
            | (false, true) => ExactlyThrice
            | (false, false) => NoMatch
        }
    }
};

let countDuplicates = (inventorySkus) => {
    let rec aux = (twos, threes, remaining) => {
        switch(remaining) {
            | [] => twos * threes
            | [hd, ...tl] => {
                let result = findDuplicateChars(hd);
                switch(result) {
                    | NoMatch => aux(twos, threes, tl)
                    | BothTwiceAndThrice => aux(twos + 1, threes + 1, tl)
                    | ExactlyThrice => aux(twos, threes + 1, tl)
                    | ExactlyTwice => aux(twos + 1, threes, tl)
                }
            }
        }
    }
    aux(0, 0, inventorySkus);
}

Js.log(countDuplicates(inventoryList));
Js.log(findCorrectSkus(inventoryList));