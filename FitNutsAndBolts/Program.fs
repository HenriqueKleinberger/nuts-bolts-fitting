module Exercice
    type ToolType =
    | Nut
    | Bolt

    type Item = {
        Size : int
        ToolType: ToolType
    }

    let FitBoltsAndNuts = fun (items: Item list) ->
        let (bolts, nuts) = List.partition (fun (item : Item) -> item.ToolType = ToolType.Bolt) items

        let PartitionSmallerEqualBigger (list: Item list) (pivot: Item) =
            let rec loop (l: Item list) (i: Item)=
                match l with
                    | [] -> [], [], []
                    | firstItem::othersItems ->
                        let l1, l2, l3 = loop othersItems pivot
                        if firstItem.Size < i.Size then firstItem::l1, l2, l3
                        elif firstItem.Size > i.Size then l1, l2, firstItem::l3
                        else l1, firstItem::l2 ,l3
            loop list pivot

        let rec FitBoltsAndNuts = fun (bolts: Item list) (nuts: Item list) ->
            match bolts with
            | [] -> []
            | boltPivot::othersBolts ->
                // arrange the nuts
                let (smallerNuts, equalsNuts, biggerNuts) = PartitionSmallerEqualBigger nuts boltPivot

                // take the equal nut and arrange the bolts
                let nutPivot = equalsNuts.Head;
                let (smallerBolts, equalsBolts, biggerBolts) = PartitionSmallerEqualBigger othersBolts nutPivot

                // recursion
                let smaller = FitBoltsAndNuts smallerBolts smallerNuts
                let bigger = FitBoltsAndNuts biggerBolts biggerNuts

                let equals = List.zip (boltPivot::equalsBolts) equalsNuts

                // concat the arrays
                smaller@equals@bigger
        FitBoltsAndNuts bolts nuts
