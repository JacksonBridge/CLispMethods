;Assignment6.lisp
;Jackson Hambridge
;This program defines a series of methods performing various specified tasks.



;This method constructs a list with the cons function
(defun myList()
    (cons 4 (cons (cons 7 (cons 22 ()))(cons "art" (cons (cons "math" (cons (cons 8 ()) (cons 99 ()))) (cons 100 ())))))
)



;This method can be called without parameters to output leap years between 1800 and 2019
(defun leapYear(&optional yearList year)
    (
    ;If year is empty, this is our first call
    if(null year)
    ;If this is our first call, input the optional parameters
    (leapYear nil 1800)
    ;If this isn't our first call, check if year is less than 2019
    (if(< year 2019)
        ;Check if year is divisible by 100
        (if (= (mod year 100) 0)
            ;Check if year is divisible by 400
            (if (= (mod year 400) 0)
                ;Add it to our list if it is
                (leapYear (append yearList (cons year())) (+ year 4))
                ;Skip it
                (leapYear yearList (+ year 4))
            )
            ;Call the function again, adding four to the year.
            (leapYear (append yearList (cons year())) (+ year 4))
        )
        ;Return yearList if we're past 2019
        (RETURN-FROM leapYear yearList)
    )
    )
)



;union- function
;This function takes the union of two sets.  If isIn is not null, this function acts as a duplicate checker.
(defun union-(a b &optional c isIn element)(
    ;If isIn is null, the function takes the union.
    if(null isIn)
    (
        ;If we finished traversing a
        if(null a)
        (
            ;If we finished traversing b
            if(null b)
            (
                ;Return c
                RETURN-FROM union- c
            )
            (
                ;If the element is already in c
                if(union- a b c 1 (car b))
                (
                    ;Skip the element
                    union- a (cdr b) c
                )
                (
                    ;Add the element to c
                    union- a (cdr b) (append c (cons (car b)()))
                )
            )
        )
        (
            ;If the element is already in c
            if(union- a b c 1 (car a))
            (
                ;Skip the element
                union- (cdr a) b c
            )
            (
                ;Add the element to c
                union- (cdr a) b (append c (cons (car a)()))
            )
        )
    )
    ;If isIn is not null, the list c is traversed and the program tests is element is already in c
    (
        ;If we finished traversing c
        if(null c)
        (
            ;Return false
            < 1 0
        )
        (
            ;If the element is found in c
            if(eq element (car c))
                (
                    ;Return true
                    > 1 0
                )
                (
                    ;Otherwise traverse c further
                    union- a b (cdr c) 1 element
                )
        )
    )
))



;This function takes in a list and calculates the average
(defun avg(aList &optional total count)(
    ;Check if list is null
    if(null aList)
    (
        ;Check if count is null
        if(null count)
        ;This means we were not given aList to begin with. Return nil and end function.
        (nil)
        ;Otherwise, return total over count (average)
        (/ total count)
    )
    ;If the list is not null, we traverse it
    (
        ;If count is null, this is our first call.
        if(null count)
        ;Call the function with optional parameters
        (avg aList 0 0)
        ;Add to total and count, traverse the list, and call the function again
        (avg (cdr aList) (+ total (car aList)) (+ count 1))
    )
    )
)



;isType will take in a dataType and return a lambda function that tests input for that dataType
(defun isType(dataType)(
    ;This is the lambda function.  It uses the typep command to compare the input of the lambda function to the dataType
    lambda (x)
        (typep x dataType)
    )
)



;This function calculates taxes if they are over a certain limit
(defun taxCalculator(limit rate values &optional taxedValues)(
    ;If values is empty, we traversed the list fully.
    if(null values)
    ;Return taxedValues
    (RETURN-FROM taxCalculator taxedValues)
    (
        ;If the element we are traversing is greater than the limit,
        if(> (car values) limit)
        (
            ;Calculate tax then call the function again
            taxCalculator limit rate (cdr values) (append taxedValues (cons (* rate (car values))()))
        )
        (
            ;Call the function again without calculating tax
            taxCalculator limit rate (cdr values) (append taxedValues (cons (car values)()))
        )
    )
))



;This function takes in a function and returns a list based off aList, only if the values in aList pass the function.
(defun clean(aFunc aList &optional finalList)(
    ;If aList is empty, we traversed the entire list
    if(null aList)
    ;Return the finalList
    (RETURN-FROM clean finalList)
    (
        ;Tests if the element is a list
        if(typep (car aList) 'cons)
        ;Element is a list
        (
            clean aFunc (cdr aList) (append finalList (cons (clean aFunc (car aList)) ()) )
        )
        ;Element is not a list
        (
            ;If the current element passes the function
            if(funcall aFunc (car aList))
            (
                ;If the finalList is a list (This is for formatting)
                if(typep finalList 'cons)
                ;Append with final list
                (clean aFunc (cdr aList) (append finalList (cons (car aList)() ) ) )
                ;Otherwise, construct a list
                (clean aFunc (cdr aList) (cons (car aList)() ) )
            )
            ;Otherwise, skip it
            (clean aFunc (cdr aList) finalList)
        )
    )
))



;threeWayBranch takes in two numbers and a list of three executable sublists.  Depending on the comparison of numbers, one of the three sublists is executed.
(defmacro threeWayBranch (x y toExecute)(
    ;If the numbers are equal,
    if(= x y)
        ;Execute the last statement
        (progn ``(,,@(car(cdr(cdr toExecute)))))
        (
            ;If x > y
            if(> x y)
            ;Execute the second statement
            (progn ``(,,@(car(cdr toExecute))))
            ;If x < y, execute the first statement
            (progn ``(,,@(car toExecute)))
        )
))