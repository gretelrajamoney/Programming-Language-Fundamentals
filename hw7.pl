/* Gretel Rajamoney */
/* Date: 3-08-2022 */
/* Course: CS 381 */
/* Assignment: Homework 7 */


/* course(course_number, course_name, credits) */

course(cs101,python, 2).
course(mth210, calculusI, 5).
course(cs120, web_design, 3).
course(cs200, data_structures, 4).
course(cs210, algorithms, 4).
course(wrt101, basic_writing, 3).

/* section(CRN, course_number) */

section(1522,cs101).
section(1122,cs101).
section(2322,mth210).
section(2421,cs120).
section(8522,mth210).
section(1621,cs200).
section(7822,mth210).
section(2822,cs210).
section(3522,wrt101).

/* place( CRN, building, time) */

place(1522,owen102,10).
place(1122,dear118,11).
place(2322,with210,11).
place(2421,cov216,15).
place(8522,kec1001,13).
place(1621,cov216,14).
place(7822,kec1001,14).
place(2822,owen102,13).
place(3522,with210,15).

/* enroll(sid, CRN) */

enroll(122,1522).
enroll(122,8522).
enroll(150,1522).
enroll(150,2421).
enroll(212,7822).
enroll(300,2822).
enroll(300,8522).
enroll(310,3522).
enroll(310,8522).
enroll(310,1621).
enroll(175,2822).
enroll(175,7822).
enroll(175,3522).
enroll(410,1621).
enroll(410,7822).
enroll(113,3522).

/* student(sid, student_name, major) */

student(122, mary, cs).
student(150, john, math).
student(212, jim, ece).
student(300, lee, cs).
student(310, pat, cs).
student(175, amy, math).
student(410, john, cs).
student(113, zoe, ece).



/* question 1 part a */
/* the predicate uses SID (S) to access the enroll database and access the CRN (CRN) */
/* utilizing this CRN (CRN), we can access the course_number (CN) from the section database */
/* utilizing the course_number (CN), we can access the course_name (C) from the course database */
/* using the CRN (CRN), we can also access the building (B) and the time (T) */
schedule(S, C, B, T) :- enroll(S, CRN), section(CRN, CN), course(CN, C, _), place(CRN, B, T).

/* question 1 part b */
/* the predicate uses SID (S) to access the student_name (N) from the students database */
/* using the SID (S), we can also access the CRN (CRN) from the enroll database */
/* using the acquired CRN (CRN), we can find the course_number (CN) from the section database */
/* finally, we can use the course_number (CN) to find the course_name (C) */
schedule(S, N, C) :- student(S, N, _), enroll(S, CRN), section(CRN, CN), course(CN, C, _).

/* question 1 part c */
/* the predicate uses course_number (CN) to access the course_name (N) from the course database */
/* using the course_number (CN) we can also access all the CRNs (C) from the section database */
/* lastly, using the CRNs (C), we can access the times (T) from the place database */
offer(CN, N, C, T) :- course(CN, N, _), section(C, CN), place(C, _, T).

/* question 1 part d */
/* the predicate uses SID (S) in order to output all CRN's in which classes conflict X, Y */
/* it first accesses the enroll database using SID (S), and stores the CRN in X */
/* it then finds the time (T1) from database place using the CRN stored in X */
/* it next accesses the enroll database using SID (S), and stores the CRN in Y */
/* it then finds the time (T2) from database place using the CRN stored in Y */
/* finally, it checks if the times T1 and T2 conflict with each other using =:= */
/* to ensure X and Y are not grabbing the same CRN, we check that they are not equal as well */
conflict(S, X, Y) :- enroll(S, X), place(X, _, T1), enroll(S, Y), place(Y, _, T2), T1 =:= T2, X \= Y.

/* question 1 part e */
/* the predicate uses SID (S1) in order to output all other SID (S2) that can meet with S1 */
/* it accesses all CRNs (CRN1) in which SID (S1) is enrolled in from the enroll database */
/* it accesses all CRNs (CRN2) in which SID (S2) is enrolled in from the enroll database */
/* it then checks whether both students are in the course by comparing CRN1 to CRN2 */
/* it then ensures that its not the same student by comparing the SIDs using S1 and S2 */
/* it then accesses all buildings (B1) and times (T1) for the S1 having CRNs CRN1 */
/* it then accesses all buildings (B2) and times (T2) for the S2 having CRNs CRN2 */
/* next, it checks whether buildings B1 and B2 are the same, and that S1 is not equal to S2 */
/* lastly it checks whether the classes meet back to back using T1 + 1 and T2 + 1 */
meet(S1, S2) :- enroll(S1, CRN1), enroll(S2, CRN2), CRN1 =:= CRN2, S1 \= S2.
meet(S1, S2) :- enroll(S1, CRN1), place(CRN1, B1, T1), enroll(S2, CRN2), place(CRN2, B2, T2), B1 = B2, S1 \= S2, (T1 =:= T2 + 1 ; T2 =:= T1 + 1).

/* question 1 part f */
/* the predicate uses CRN (CRN) to access the SIDs (S) of all students enrolled */
/* next, using the SIDs (S), it accesses the student database to output all student_names */
roster(CRN, Sname) :- enroll(S, CRN), student(S, Sname, _).

/* question 1 part g */
/* the predicate accesses the course database to output all course_names with credits >= 4 */
highCredits(Cname) :- course(_, Cname, C), C >= 4.


/* question 2 part a */
/* the predicate rdup elimates all duplicates found in the ordered list L */
/* if the start of the list is not a member of M, it appends it to the list R */
/* next we recursively call rdup with the end of the list to move it forward */
/* if the start of the list is a member of M, it skips it and continues through the list */
/* the resulting list containing no duplicates is then outputted */
rdup([], L, L).
rdup([S|E], M, L) :- (not(member(S, M)) -> append(M, [S], R), rdup(E, R, L) ; rdup(E, M, L)).
rdup(T, L) :- rdup(T, [], L).

/* question 2 part b */
/* the predicate flat takes in a list of elements and outputs a singular list in order */
/* if the start of the list is not a list, it appends it to the R list and continues */
/* it then recursively calls the flat predicate instead inputting the end of the list */
/* if the start of the list is a list, it appends it to the U list and continues */
/* it then recursively calls the flat predicate instead inputting the end of the list */
/* the resulting list contains a flat list containing a singular list that is in order */
flat([], L, L).
flat([S|E], F, L) :- (not(is_list(S)) -> append(F, [S], R), flat(E, R, L) ; flat(S, Y), append(F, Y, U), flat(E, U, L)).
flat(T, L) :- flat(T, [], L).

/* question 2 part c */
/* the predicate takes in a list containing positions, and a list containing all elements */
/* if the counter which has been initialized to 1 is equal to the start of the elements */
/* then the element from the list is appended into the results list X */
/* the counter is then incremented by adding 1 in order to access the next index */
/* the predicate is then recursively called using the new counter and next element */
/* if the counter is not equal to the start of the elements */
/* the counter is still incremented by adding 1 in order to access the next index */
/* the predicate is also still recursively called using the new counter and next element */
/* finally, the list containing the appended elements is outputted */
project([], _, R, _, R).
project([S|E], [S1|E1], R, C, T) :- (C = S -> append(T, [S1], X), C1 is C + 1, project(E, E1, R, C1, X) ; C1 is C + 1, project([S|E], E1, R, C1, T)).
project(P, L, R) :- project(P, L, R, 1, []).






