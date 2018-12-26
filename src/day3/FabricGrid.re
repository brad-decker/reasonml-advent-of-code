type squareInch = Unclaimed | ClaimedByOne(int) | ClaimedByMultiple(list(int));
type claimResponse = Fulfilled(int) | Conflicted | ClaimNotFound;

type claim = {
    claimId: int,
    left: int,
    top: int,
    width: int,
    height: int,
};

let make = () => Array.make_matrix(1000, 1000, Unclaimed);
let updateContents = id => fun
    | Unclaimed => ClaimedByOne(id)
    | ClaimedByOne(originalId) => ClaimedByMultiple([id, originalId])
    | ClaimedByMultiple(ids) => ClaimedByMultiple([id, ...ids]);

let fillRow = (claimId, start, numberOfInches, row) => {
    let current = Array.sub(row, start, numberOfInches);
    let updaterFn = updateContents(claimId);
    let newValues = Array.map(updaterFn, current);
    ArrayLabels.blit(~src=newValues, ~src_pos=0, ~dst_pos=start, ~dst=row, ~len=numberOfInches);
    row;
}

let claimFabric = (claimId, left, top, width, height, grid) => {
    let current = Array.sub(grid, top, height);
    let updaterFn = fillRow(claimId, left, width);
    let newValues = Array.map(updaterFn, current);
    ArrayLabels.blit(~src=newValues, ~src_pos=0, ~dst=grid, ~dst_pos=top, ~len=height);
    grid;
}

let getNumericOperator = (cell) => {
    switch(cell) {
        | Unclaimed => 0
        | ClaimedByOne(_) => 0
        | ClaimedByMultiple(_) => 1
    }
}

let countMultiples = (grid: array(array(squareInch))) => {
    let gridList = Array.to_list(Array.map(Array.to_list, grid));
    let rec countGrid = (remainingRows, count) => fun
        | [] when List.length(remainingRows) > 0 => countGrid(List.tl(remainingRows), count, List.hd(remainingRows))
        | [] => count
        | [hd] =>  countGrid(remainingRows, count + getNumericOperator(hd), [])
        | [hd, ...tl] => countGrid(remainingRows, count + getNumericOperator(hd), tl)
    
    countGrid(List.tl(gridList), 0, List.hd(gridList));
}

let getClaimCells = (left, top, width, height, grid: array(array(squareInch))) => {
    let rows = Array.sub(grid, top, height);
    rows
        |> Array.map(r => Array.sub(r, left, width))
        |> Array.fold_left((a, b) => Array.append(a, b), [||])
        |> Array.to_list;
}

let checkClaim = (claim: claim, grid) => {
    let cells = getClaimCells(claim.left, claim.top, claim.width, claim.height, grid);
    cells |> List.for_all(a => {
        switch(a) {
            | Unclaimed => false
            | ClaimedByMultiple(ids) => false
            | ClaimedByOne(id) when id == claim.claimId => true
            | ClaimedByOne(_) => false
        }
    });
}

let findFulfilled = (claims, grid) => {
    let rec checkFulfilled = (claimsToCheck) => {
        if (List.length(claimsToCheck) == 0) {
            raise(Not_found)
        }
        let claim = List.hd(claimsToCheck);
        let remaining = List.tl(claimsToCheck);
        switch(checkClaim(claim, grid)) {
            | true => claim
            | false => checkFulfilled(remaining)
        }
    }
    checkFulfilled(claims);
}

let y = make();
