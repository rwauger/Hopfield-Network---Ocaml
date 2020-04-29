let nextState = function(currentState, weightMatrix) ->
	if(currentState == [])
		then []
	else
	    hop11ActAll(netAll(currentState,weightMatrix), currentState);;

let rec updateN = function (currentState, weightMatrix, n)->
    if(n==0)
		then currentState
	else
        updateN(nextState(currentState,weightMatrix) , weightMatrix , n-1);;

let findsEquilibrium = function(initialState, weightMatrix, range)->
	if(range == 0)
		then false
	else
		if(updateN(initialState, weightMatrix, range) = updateN(initialState, weightMatrix, range-1))
			then true
		else
			false;;

let energy = function(state,weightMatrix) ->
	if(weightMatrix==[])
		then 0.
	else
		-(0.5) *. (inner(netAll(state, weightMatrix), state));;

let rec ms = function(s, v, pos, n)->
    if s == []
        then []
    else if n == pos
        then 0. :: ms(tl s, v, pos, n+1)
    else
        (hd s *. v) :: ms(tl s, v, pos, n+1);;

let rec outer = function(v1, v2, pos)->
    if v1 = []
        then []
    else
        ms(v2, hd v1, pos, 0) :: outer(tl v1, v2, pos+1);;

let hopTrainAstate = function(astate)->
    outer(astate, astate, 0);;
    
let rec sum = function (v1, v2) ->
if v1 = [] then
[]
else
List.hd v1 +. List.hd v2 :: sum (List.tl v1, List.tl v2);;

let rec sumMat = function (v1, v2) ->
    if v2 = [] then
        v1
    else
        sum (List.hd v1, List.hd v2) :: sumMat (List.tl v1, List.tl v2);;

let rec hopTrain = function (allStates) ->
    if allStates = [] then
        []
    else
        sumMat (hopTrainAstate (List.hd allStates), hopTrain (List.tl allStates));;
