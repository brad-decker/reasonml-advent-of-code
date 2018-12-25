let frequencyList = Utilities.fileToListByNewLines(~dir=[%bs.node __dirname], "./input.txt");

let table = Hashtbl.create(List.length(frequencyList) / 2);

let rec findFirstDuplicate = (frequencies, current) => {
    switch(frequencies) {
        | [] => findFirstDuplicate(frequencyList, current)
        | [hd, ...tail] => {
            let newValue = current + Frequency.getFrequencyValue(hd);
            switch(Hashtbl.find(table, newValue)) {
                | v => v
                | exception(Not_found) => {
                    Hashtbl.add(table, newValue, newValue);
                    findFirstDuplicate(tail, newValue);
                }
            }
        }
    }
}

let a = findFirstDuplicate(frequencyList, 0);
Js.log(a);