

let getFrequenciesFromFile = () => {
    let input = Node_fs.readFileAsUtf8Sync("input.txt");
    let r = Js.Re.fromString("\n");
    Js.String.splitByRe(r, input);
}

let getFrequencyValue = frequency => {
    let value = String.length(frequency) - 1 |> String.sub(frequency, 1) |> int_of_string;
    switch (String.get(frequency, 0)) {
        | '-' => -value
        | _ => value
    }
};

let result = Array.fold_left((a, b) => a + getFrequencyValue(b), 0, getFrequenciesFromFile());
Js.log("Final Frequency: " ++ string_of_int(result));
