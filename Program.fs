
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

let rec fitBoltsAndNuts = fun (bolts: Item list) (nuts: Item list) ->
    match bolts with
    | [] -> []
    | boltPivot::othersBolts ->
        let partitionSmallerEqualBigger (list: Item list) (pivot: Item) =
          let rec loop (l: Item list) (i: Item)= 
            match l with 
            | [] -> [], [], []
            | firstItem::othersItems ->
                let l1, l2, l3 = loop othersItems pivot
                if firstItem.Size < i.Size then firstItem::l1, l2, l3
                elif firstItem.Size > i.Size then l1, l2, firstItem::l3
                else l1, firstItem::l2 ,l3
          loop list pivot
        
        // arrange the nuts
        let (smallerNuts, equalsNuts, biggerNuts) = partitionSmallerEqualBigger nuts boltPivot

        // take the equal nut and arrange the bolts
        let nutPivot = equalsNuts.Head;
        let (smallerBolts, equalsBolts, biggerBolts) = partitionSmallerEqualBigger othersBolts nutPivot

        // recursion
        let smaller = fitBoltsAndNuts smallerNuts smallerBolts
        let bigger = fitBoltsAndNuts biggerBolts biggerNuts

        let equals = List.zip (boltPivot::equalsBolts) equalsNuts

        smaller@equals@bigger


let result = fitBoltsAndNuts bolts nuts

// print the pairs that fits
List.map(fun ((bolt: Item), (nut: Item)) -> eprintfn $"{bolt.Type}: {bolt.Size} fits into {nut.Type}: {nut.Size}") result