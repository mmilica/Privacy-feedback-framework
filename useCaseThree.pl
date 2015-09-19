%:- tell('UseCaseThreeOUTPUT.txt').
:- [modelBase].
:- [model].
:- [disclosureWeight].
:-[addingToCollaboratorsDB].

:- write('DEBUG: university test').

:-defineCredType('X509', 'BeID', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
								attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).
								
:-defineCredType('X509', 'BeDL', [attrDef('Subject', 'String', 'Known', 'Assigned', 'Shared'),
								attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique'),
								attrDef('VehicleClass', 'Letter', 'Known', 'Assigned', 'Shared')]).
								
:-defineCredType('Idemix', 'loyalty credential', [attrDef('Id', 'String', 'Known', 'Chosen', 'Unique'),
	                           attrDef('Rand', 'Integer', 'Chosen', 'Hidden', 'Shared'),
                              attrDef('SysRand', 'Integer', 'Known', 'Chosen', 'Shared'),
                              attrDef('Gender', 'Letter', 'Known', 'Assigned', 'Shared'),
                              attrDef('ZIP', 'ZipCode', 'Known', 'Assigned', 'Shared'),
                              attrDef('YearOfBirth', 'Year', 'Known', 'Assigned', 'Shared')
                             ]).
							 
:-defineCredType('X509', 'bank card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Address', 'String', 'Known', 'Assigned', 'Shared (limited)'),
									attrDef('DoB', 'Date', 'Known', 'Assigned', 'Shared'),
									attrDef('PK', 'PublicKey', 'Chosen', 'Known', 'Unique')]).
			
	
:-defineCredType('X509', 'student card', [attrDef('Name', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('Surname', 'String', 'Known', 'Assigned', 'Shared'),
									attrDef('StudentNumber', 'Integer', 'Known', 'Assigned', 'Unique'),
									attrDef('University', 'String', 'Known', 'Assigned', 'Shared')]).

			
%:- assert(isUnique('DoB', credSource('Idemix', 'loyalty credential', 'gov'))).
			
:-assert(uniqueSet([attrNameSourcePair('Name', credSource('X509', 'BeID', 'gov')), attrNameSourcePair('Surname', credSource('X509', 'BeID', 'gov'))])).					
%:-assert(isUnique('Name', credSource('X509', 'BeID', 'gov'))).
%:-assert(isUnique('Surname', credSource('X509', 'BeID', 'gov'))).

%:-assert(isUnique('StudentNumber', credSource('X509', 'student card', 'KU Leuven'))).
:-assert(uniqueSet([attrNameSourcePair('StudentNumber', credSource('X509', 'student card', 'KU Leuven')), attrNameSourcePair('DoB', credSource('X509', 'BeID', 'gov'))])).					


/*Providers' collaboration*/
:-assert(collaboratingSPs('AdsProvider', 'OnlineShop')).
:-assert(collaboratingSPs('OnlineShop', 'AdsProvider')).


/*Existing profile entries*/
:- 	assert(transactionID('99')).
:- assert(dynamicProfile('AdsProvider', '99', attr('StudentNumber', '1234'), credSource('X509', 'student card', 'KU Leuven'))).
:- assert(dynamicProfile('AdsProvider', '99', attr('DoB', '1980'), credSource('X509', 'BeID', 'gov'))).


	
:- issueCred('Alice', 'My Belgian ID card', 'X509', 'BeID', 'gov', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('Address', 'Leuven'), 
						attr('DoB', '1980'), attr('PK', '12345')], Cred).


:- issueCred('Alice', 'MyStudentCard', 'X509', 'student card', 'KU Leuven', '2015', [attr('Name', 'Jane'), 
						attr('Surname', 'Johnson'), attr('StudentNumber', '1234'), 
						attr('University', 'KU Leuven')], Cred1).

				
		
		
		

%:-showCred('Alice', 'OnlineShop', '1', cred('MyStudentCard', _, _, _), ['University']).
:-showCred('Alice', 'OnlineShop', '1', cred('MyStudentCard', _, _, _), ['Ownership']).


%:- showCred('Alice', 'OnlineShop', '2', cred('My Belgian ID card', _, _, _), ['DoB']).
:- userInput('Alice', 'OnlineShop', '1', ['Purchase2']).

/*-------------------------------------------------------------
|	Structure:
|		assert(attributeWeight(Type, Technology, Issuer, AttributeName, Weight)).
| -------------------------------------------------------------*/

:- assert(attributeWeight('BeID', 'X509', 'gov', 'IssuerSig', 0)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'Issuer', 0)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'Name', 1)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'Surname', 2)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'Address', 2)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'DoB', 2)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'PK', 0)).
:- assert(attributeWeight('BeID', 'X509', 'gov', 'ExpiryDate', 0)).

:- showCredCount('Alice', 'OnlineShop', '1', cred('My Belgian ID card', _, _, _), ['DoB']).



:- nl, nl, write('-------------------------------------------------'), nl, write('FINISHED'), nl, 
	write('---------------------------------------------------').

	
/*------------------------------------------
%
%		Queries for creating the results:
%
%		isLinked(A, B, C, D).
%
%		profile(A, B, C, D).
%
------------------------------------------*/