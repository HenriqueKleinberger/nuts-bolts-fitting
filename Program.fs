
type Type =
| Nut
| Bolt

type Item = {
    Size : int
    Type: Type
}

let bag = [
    { Type = Nut; Size = 10 }
    { Type = Bolt; Size = 10 }
    { Type = Nut; Size = 5 }
    { Type = Bolt; Size = 12 }
    { Type = Nut; Size = 12 }
    { Type = Nut; Size = 18 }
    { Type = Bolt; Size = 18 }
    { Type = Nut; Size = 3 }
    { Type = Bolt; Size = 5 }
    { Type = Bolt; Size = 3 }
    { Type = Bolt; Size = 3 }
    { Type = Nut; Size = 3 }
]

let (bolts, nuts) = List.partition (fun (item : Item) -> item.Type = Type.Bolt) bag

let rec findBoltsAndNuts = fun (bolts: Item list) (nuts: Item list) ->
    if not nuts.IsEmpty then
        
        let partitionSmallerEqualBigger (list: Item list) (pivot: Item) =
          let rec loop (l: Item list) (i: Item)= 
            match l with 
            | [] -> [], [], []
            | firstItem::othersItems ->
                let l1, l2, l3 = loop othersItems firstItem
                if firstItem.Size < i.Size then firstItem::l1, l2, l3
                elif firstItem.Size > i.Size then l1, l2, firstItem::l3
                else l1, firstItem::l2 ,l3
          loop list pivot

        // take the first nut and arrange the bolts
        let nutPivot = nuts.Head;
        let (smallerBolts, equalsBolts, biggerBolts) = partitionSmallerEqualBigger bolts nutPivot

        // take one of the bolts that fits and arrange the nuts
        let boltPivot = equalsBolts.Head
        let (smallerNuts, equalsNuts, biggerNuts) = partitionSmallerEqualBigger nuts boltPivot

        // print the pairs that fits
        if equalsBolts.Length = equalsNuts.Length then
            for i in 0 .. equalsBolts.Length - 1 do
                 eprintfn $"bolt: {equalsBolts[i].Size}, nut: {nuts[i].Size}"
        else 
            eprintfn $"the sizes are not the same"

        // recursion
        findBoltsAndNuts smallerNuts smallerBolts
        findBoltsAndNuts biggerBolts biggerNuts


findBoltsAndNuts bolts nuts