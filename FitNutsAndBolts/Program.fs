module Exercice
type ToolType =
| Nut
| Bolt

type Item = {
    Size : int
    ToolType: ToolType
}

let private partitionSmallerEqualBigger (list: Item list) (pivot: Item) =
    let rec loop (l: Item list) (i: Item)= 
        match l with 
            | [] -> [], [], []
            | firstItem::othersItems ->
                let l1, l2, l3 = loop othersItems pivot
                if firstItem.Size < i.Size then firstItem::l1, l2, l3
                elif firstItem.Size > i.Size then l1, l2, firstItem::l3
                else l1, firstItem::l2 ,l3
    loop list pivot

let fitBoltsAndNuts = fun (items: Item list) ->
    let (bolts, nuts) = List.partition (fun (item : Item) -> item.ToolType = ToolType.Bolt) items

    let rec fitBoltsAndNuts = fun (bolts: Item list) (nuts: Item list) ->
        match bolts with
        | [] -> []
        | boltPivot::othersBolts ->        
            // arrange the nuts
            let (smallerNuts, equalsNuts, biggerNuts) = partitionSmallerEqualBigger nuts boltPivot

            // take the equal nut and arrange the bolts
            let nutPivot = equalsNuts.Head;
            let (smallerBolts, equalsBolts, biggerBolts) = partitionSmallerEqualBigger othersBolts nutPivot

            // recursion
            let smaller = fitBoltsAndNuts smallerBolts smallerNuts
            let bigger = fitBoltsAndNuts biggerBolts biggerNuts

            let equals = List.zip (boltPivot::equalsBolts) equalsNuts

            // concat the 
            smaller@equals@bigger
    fitBoltsAndNuts bolts nuts
