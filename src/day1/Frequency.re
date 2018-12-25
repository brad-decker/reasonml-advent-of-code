
let getFrequencyValue = frequency => {
    let value = String.length(frequency) - 1 |> String.sub(frequency, 1) |> int_of_string;
    switch (String.get(frequency, 0)) {
        | '-' => -value
        | _ => value
    }
};

let result = List.fold_left((a, b) => a + getFrequencyValue(b), 0, Utilities.fileToListByNewLines(~dir=[%bs.node __dirname], "input.txt"));
Js.log("Final Frequency: " ++ string_of_int(result));
