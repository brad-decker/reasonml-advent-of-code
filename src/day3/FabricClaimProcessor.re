let grid = FabricGrid.make();
let claims = Utilities.fileToListByNewLines(~dir=[%bs.node __dirname], "input.txt");

let r = Js.Re.fromString("#(\d+) @ (\d+),(\d+): (\d+)x(\d+)");

let getClaimParts = (claim): FabricGrid.claim => {
    switch(Js.String.match(r, claim)) {
        | Some(matches) => {
            claimId: int_of_string(matches[1]),
            left: int_of_string(matches[2]),
            top: int_of_string(matches[3]),
            width: int_of_string(matches[4]),
            height: int_of_string(matches[5]),
        }
        | None => raise(Not_found)
    }
}

let parsedClaims = List.map(a => {
    let claim = getClaimParts(a);
    FabricGrid.claimFabric(claim.claimId, claim.left, claim.top, claim.width, claim.height, grid);
    claim;
}, claims);


Js.log(FabricGrid.countMultiples(grid));
/* Js.log(List.length(parsedClaims)); */
Js.log(FabricGrid.findFulfilled(parsedClaims, grid));