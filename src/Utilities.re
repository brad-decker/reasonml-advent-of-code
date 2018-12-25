let fileToListByNewLines = (~dir=[%bs.node __dirname], file) => {
    let dirname = switch(dir) {
        | Some(v) => v
        | None => ""
    };
    let path = Node.Path.join([|dirname, file|]);
    Js.log(path);
    let input = Node_fs.readFileAsUtf8Sync(path);
    let r = Js.Re.fromString("\n");
    Js.String.splitByRe(r, input) |> Array.to_list;
}

/* Sort Alphabetically  */
let alphaSort = (a, b) => {
    switch (a >= b) {
        | true when a == b => 0
        | true => 1
        | false => -1
    }
};

/* Explode string into list of chars */
let explode = (s) => {
    let rec exp = (i, l) =>
        if (i < 0) {
            l;
        } else {
            exp(i - 1, [s.[i], ...l]);
        };
    exp(String.length(s) - 1, []);
};