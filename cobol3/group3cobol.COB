       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. prog.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT CUSTOMER ASSIGN TO 'customer.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRODUCT ASSIGN TO 'product.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT C_ORDER ASSIGN TO 'order.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT ORDERLINE ASSIGN TO 'orderline.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
           FD CUSTOMER.
           01 CUSTOMER-FILE.
               05 CST-ID PIC 9(5).
               05 CST-NAME PIC A(15).
           FD PRODUCT.
           01 PRODUCT-FILE.
               05 PRD-ID PIC 9(5).
               05 PRD-NAME PIC A(10).
               05 PRD-PRICE PIC 9(2)V9(2).
           FD C_ORDER.
           01 ORDER-FILE.
               05 ORD-ID PIC 9(5).
               05 ORD-DATE PIC 9(8).
               05 ORD-CST-ID PIC 9(5).
           FD ORDERLINE.
           01 ORDERLINE-FILE.
               05 ORDL-ID PIC 9(5).
               05 ORDL-PRD-ID PIC 9(5).
               05 ORDL-QUAN PIC 9(2).
      *-----------------------
       WORKING-STORAGE SECTION.
           01 WS-CNT PIC 9(1) VALUE 0.
           01 WS-CHC PIC 9(1) VALUE 0.
           01 WS-CST.
               05 WS-CST-ID PIC 9(5).
               05 WS-CST-NAME PIC A(15).
           01 WS-PRD.
               05 WS-PRD-ID PIC 9(5).
               05 WS-PRD-NAME PIC A(10).
               05 WS-PRD-PRICE PIC 9(2)V9(2).
           01 WS-ORD.
               05 WS-ORD-ID PIC 9(5).
               05 WS-ORD-DATE PIC 9(8).
               05 WS-ORD-CST-ID PIC 9(5).
           01 WS-ORDL.
               05 WS-ORDL-ID PIC 9(5).
               05 WS-ORDL-PRD-ID PIC 9(5).
               05 WS-ORDL-QUAN PIC 9(2).
           01 WS-EOF PIC A(1).
           01 WS-TEMP-EOF PIC A(1).
           01 WS-TEMP-PR-EOF PIC A(1).
           01 WS-INP-CST-ID PIC 9(5).
           01 WS-INP-CST-ID-FND PIC A(1).
           01 WS-INP-PRD-ID PIC 9(5).
           01 WS-INP-PRD-ID-FND PIC A(1).
           01 WS-INP-PRD-PRICE PIC 9(2)V9(2).
           01 WS-INP-QUAN PIC 9(2).
           01 WS-PRD-COST PIC Z(3)9.99.
           01 WS-ORD-COUNT PIC 99.
           01 WS-INP-ORD-DATE PIC 9(8).
           01 WS-ORD-NONE PIC A(1).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           MOVE 0 TO WS-CNT.
           PERFORM CHOICES-PROCEDURE WITH TEST AFTER UNTIL WS-CNT=1.
           STOP RUN.
       CHOICES-PROCEDURE.
           MOVE 'N' TO WS-EOF.
           MOVE 'N' TO WS-INP-CST-ID-FND.
           MOVE 'N' TO WS-INP-PRD-ID-FND.
           MOVE 'Y' TO WS-ORD-NONE.
           MOVE 0 TO WS-ORD-COUNT.
           DISPLAY 'OPTIONS:'.
           DISPLAY '1. View all customers'.
           DISPLAY '2. View all products'.
           DISPLAY "3. View a customer's orders".
           DISPLAY '4. Create an order'.
           DISPLAY '5. Exit'.
           DISPLAY 'Enter option number: '.
           ACCEPT WS-CHC.
           EVALUATE WS-CHC
               WHEN 1
                   DISPLAY 'List of customers:'
                   PERFORM CUSTOMER-PROCEDURE
               WHEN 2
                   DISPLAY 'List of products:'
                   PERFORM PRODUCT-PROCEDURE
               WHEN 3
                   PERFORM ORDER-PROCEDURE
               WHEN 4
                   DISPLAY 'Create order:'
                   PERFORM CREATE-ORDER-PROCEDURE
               WHEN 5
                   DISPLAY 'Closing program...'
                   MOVE 1 TO WS-CNT
               WHEN OTHER
                   DISPLAY 'Invalid. Please input a valid number.'
           END-EVALUATE.

       CUSTOMER-PROCEDURE.
           DISPLAY 'CSTID CSTNAME'
           OPEN INPUT CUSTOMER.
           PERFORM UNTIL WS-EOF='Y'
               READ CUSTOMER INTO WS-CST
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END DISPLAY WS-CST-ID' 'WS-CST-NAME
               END-READ
           END-PERFORM
           CLOSE CUSTOMER.

       PRODUCT-PROCEDURE.
           DISPLAY'PRDID   PRDNAME  PRICE'
           OPEN INPUT PRODUCT.
           PERFORM UNTIL WS-EOF='Y'
               READ PRODUCT INTO WS-PRD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END DISPLAY WS-PRD-ID' 'WS-PRD-NAME' '
                   WS-PRD-PRICE
               END-READ
           END-PERFORM
           CLOSE PRODUCT.

       CREATE-ORDER-PROCEDURE.
           DISPLAY 'Enter customer ID:'.
           ACCEPT WS-INP-CST-ID.
           OPEN INPUT CUSTOMER
           PERFORM UNTIL WS-EOF='Y'
               READ CUSTOMER INTO WS-CST
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-INP-CST-ID = WS-CST-ID THEN
                       MOVE 'Y' TO WS-INP-CST-ID-FND
                   END-IF
               END-READ
           END-PERFORM
           CLOSE CUSTOMER
           EVALUATE WS-INP-CST-ID-FND
               WHEN 'Y'
                   PERFORM GET-PRODUCT-ORDER-PROCEDURE
               WHEN 'N'
                   DISPLAY 'Customer does not exist'
               WHEN OTHER
                   DISPLAY 'Error'
           END-EVALUATE.

       GET-PRODUCT-ORDER-PROCEDURE.
           MOVE 'N' TO WS-EOF.
           DISPLAY 'Enter product ID:'
           ACCEPT WS-INP-PRD-ID
           OPEN INPUT PRODUCT
           PERFORM UNTIL WS-EOF='Y'
               READ PRODUCT INTO WS-PRD
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END
                   IF WS-INP-PRD-ID = WS-PRD-ID THEN
                       MOVE 'Y' TO WS-INP-PRD-ID-FND
                       MOVE WS-PRD-PRICE TO WS-INP-PRD-PRICE
                   END-IF
               END-READ
           END-PERFORM
           CLOSE PRODUCT
           EVALUATE WS-INP-PRD-ID-FND
               WHEN 'Y'
                   PERFORM GET-QUANTITY-ORDER-PROCEDURE
               WHEN 'N'
                   DISPLAY 'Product does not exist'
               WHEN OTHER
                   DISPLAY 'Error'
           END-EVALUATE.

       GET-QUANTITY-ORDER-PROCEDURE.
           DISPLAY 'Enter quantity:'
           ACCEPT WS-INP-QUAN
           MULTIPLY WS-INP-PRD-PRICE BY WS-INP-QUAN
               GIVING WS-PRD-COST
           DISPLAY 'Total cost of order: 'WS-PRD-COST
           PERFORM SAVE-ORDER-PROCEDURE.

       SAVE-ORDER-PROCEDURE.
           DISPLAY 'Enter order date (MMDDYYYY):'
           ACCEPT WS-INP-ORD-DATE
           MOVE 'N' TO WS-EOF
           OPEN INPUT ORDERLINE
           PERFORM UNTIL WS-EOF='Y'
               READ ORDERLINE INTO WS-ORDL
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END ADD 1 TO WS-ORD-COUNT
               END-READ
           END-PERFORM
           CLOSE ORDERLINE.
           OPEN EXTEND ORDERLINE
           MOVE WS-ORD-COUNT TO ORDL-ID
           MOVE WS-INP-PRD-ID TO ORDL-PRD-ID
           MOVE WS-INP-QUAN TO ORDL-QUAN
           WRITE ORDERLINE-FILE
           END-WRITE
           CLOSE ORDERLINE.
           OPEN EXTEND C_ORDER
           MOVE WS-ORD-COUNT TO ORD-ID
           MOVE WS-INP-ORD-DATE TO ORD-DATE
           MOVE WS-INP-CST-ID TO ORD-CST-ID
           WRITE ORDER-FILE
           END-WRITE
           CLOSE C_ORDER.

       ORDER-PROCEDURE.
           DISPLAY 'List of orders:'
           DISPLAY 'Enter customer ID:'
           ACCEPT WS-INP-CST-ID.
           DISPLAY 'ORDID   DATE   PRDID   PRDNAME  QNT TOTALCOST'
           MOVE 'N' TO WS-EOF
           OPEN INPUT C_ORDER
           PERFORM UNTIL WS-EOF='Y'
               READ C_ORDER INTO WS-ORD
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                   IF WS-INP-CST-ID = WS-ORD-CST-ID THEN
                       DISPLAY WS-ORD-ID' 'WS-ORD-DATE' 'NO ADVANCING
                       MOVE 'N' TO WS-ORD-NONE
                       PERFORM ACCESS-ORDERLINE-PROCEDURE
                   END-IF
               END-READ
           END-PERFORM
           CLOSE C_ORDER
           IF WS-ORD-NONE = 'Y' THEN
               DISPLAY 'No order'
           END-IF.

       ACCESS-ORDERLINE-PROCEDURE.
           MOVE 'N' TO WS-TEMP-EOF
               OPEN INPUT ORDERLINE
               PERFORM UNTIL WS-TEMP-EOF='Y'
               READ ORDERLINE INTO WS-ORDL
                   AT END MOVE 'Y' TO WS-TEMP-EOF
                   NOT AT END
                   IF WS-ORD-ID = WS-ORDL-ID THEN
                       DISPLAY WS-ORDL-PRD-ID' 'NO ADVANCING
                       PERFORM CHECK-PRICE-PROCEDURE
               END-READ
           END-PERFORM
           CLOSE ORDERLINE.

       CHECK-PRICE-PROCEDURE.
           MOVE 'N' TO WS-TEMP-PR-EOF
               OPEN INPUT PRODUCT
               PERFORM UNTIL WS-TEMP-PR-EOF='Y'
                   READ PRODUCT INTO WS-PRD
                   AT END MOVE 'Y' TO WS-TEMP-PR-EOF
                   NOT AT END
                   IF WS-ORDL-PRD-ID = WS-PRD-ID THEN
                       DISPLAY WS-PRD-NAME' 'WS-ORDL-QUAN' '
                       NO ADVANCING
                       MULTIPLY WS-PRD-PRICE BY WS-ORDL-QUAN GIVING
                       WS-PRD-COST
                       DISPLAY WS-PRD-COST
                   END-IF
                   END-READ
               END-PERFORM
               CLOSE PRODUCT.

       END PROGRAM prog.
