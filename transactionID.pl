
/*{+==============================================================
 |
 |  createNewTID(Provider, TID)
 |    inputs: Provider
 |    outputs: Signature
 |    effects: generates and returnes a fresh unique signature
 |
 |  --> signature is derived from Issuer and unique number
 |
 +==============================================================}*/

createNewTID(Provider, TID) :-
	getTIDCount(Counter),
	atomic_list_concat(['TID_', Provider, '_', Counter], TID).
	
/*{--------------------------------------------------------------
 | TIDCount counts the signatures 
+----------------------------------------------------------------}*/

:-dynamic(tidCount/1).
tidCount(1).
	
/*{===============================================================
 |
 |  tidCount(Count)
 |    inputs: ---
 |    outputs: Count
 |    effects: returns the current transaction ID count and updates the count
 |
 +=============================================================}*/

getTIDCount(Count) :-
	tidCount(Count),
	Count1 is Count + 1,
   retract(tidCount(_)),
	assert(tidCount(Count1)).

/*{===============================================================
 |
 |  startTransaction(User, SP, TransactionId)
 |    inputs: User, SP 
 |    outputs: TransactionId
 |    effects: creates new transactionID
 |
 +=============================================================}*/
	
startTransaction(User, SP, TransactionId):- 
	createNewTID(SP, TransactionId).