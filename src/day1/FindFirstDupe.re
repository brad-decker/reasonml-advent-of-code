let frequencyInput = Frequency.getFrequenciesFromFile();

let table = Hashtbl.create(Array.length(frequencyInput) / 2);

let frequencyList = Array.to_list(frequencyInput);

let rec findFirstDuplicate = (frequencies, current) => {
    switch(frequencies) {
        | [] => findFirstDuplicate(Array.to_list(frequencyInput), current)
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

let a = findFirstDuplicate(Array.to_list(frequencyInput), 0);
Js.log(a);