

```
select :: ∀ r a b: (r\l) => { l: a | r }  -> b -> a
select r  l  -> r.l 


(_._) :: ∀ rab: (r\l) => { l: a | r} -> Lab l -> a

```


Function to extract labels


attrKeys: {l : a} ->  [l]
select: 
