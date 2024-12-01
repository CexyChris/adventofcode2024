      ******************************************************************
      *  Dec. 1st
      *  1st Puzzle
      *
      *  Historian Hyteria - 1st try
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
        PROGRAM-ID. D01HISHY.
        AUTHOR. ChristophBuck.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SideBySide ASSIGN TO SIDBYSID
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD SideBySide RECORDING MODE F.
       01  SideBySide-Record.
           05  LeftRec            PIC 9(5) DISPLAY.
           05                     PIC X(3).
           05  RightRec           PIC 9(5) DISPLAY.

       WORKING-STORAGE SECTION.
       01  Working-Fields.
           05  MY-PGM             PIC X(8) VALUE 'D01HISHY'.
           05  File-Status        PIC 9(1) BINARY.
               88 EOF             VALUE 1
                             WHEN FALSE 0.
           05  LeftNums.
               10 LeftNum         PIC 9(8) BINARY OCCURS 1000 TIMES
                                           ASCENDING KEY IS LeftNum.
           05  RightNums.
               10 RightNum        PIC 9(8) BINARY OCCURS 1000 TIMES
                                           ASCENDING KEY IS RightNum
                                           INDEXED BY rindex.
           05  counter            PIC 9(4) BINARY.
           05  counter2           PIC 9(4) BINARY.
           05  SumOfDistances     PIC 9(8) BINARY.
           05  SimScore           PIC 9(16) BINARY.

       01  Output-Msg             PIC X(80).
       01  Result-Fields.
           05  Sum-Result         PIC 9(12) DISPLAY.
           05  SimScore-Result         PIC 9(12) DISPLAY.

      /
       PROCEDURE DIVISION.

       000-Main SECTION.
      * init
           INITIALIZE Result-Fields
           INITIALIZE Output-Msg
           MOVE ZERO TO SumOfDistances 
           MOVE ZERO TO SimScore
           MOVE 1 TO counter
           MOVE 1 TO counter2
           SET EOF TO FALSE

      * Read ahead
           OPEN INPUT SideBySide 
           READ SideBySide NEXT RECORD
                AT END SET EOF TO TRUE
           END-READ

           PERFORM UNTIL EOF

             MOVE LeftRec  TO LeftNum( counter )
             MOVE RightRec TO RightNum( counter )
             ADD 1 TO counter

             READ SideBySide  NEXT RECORD
                  AT END SET EOF TO TRUE
             END-READ
           END-PERFORM

           CLOSE SideBySide 

           SORT LeftNum 
           SORT RightNum

           PERFORM VARYING counter FROM 1 BY 1 
             UNTIL counter > 1000
                   COMPUTE SumOfDistances = SumOfDistances + 
                     FUNCTION ABS (
                           LeftNum ( counter ) - RightNum( counter )
                         )
                   END-COMPUTE
                   PERFORM VARYING counter2 FROM 1 BY 1
                     UNTIL counter2 > 1000
                         IF LeftNum (counter) EQUAL RightNum (counter2 )
                           ADD LeftNum ( counter ) TO SimScore
                         END-IF
                    END-PERFORM
      *             SEARCH ALL RightNum 
      *               WHEN RightNum ( rindex ) = LeftNum ( counter )
      *               ADD LeftNum ( counter ) TO SimScore
      *             END-SEARCH
      *             SET rindex UP BY 1
      *             PERFORM UNTIL RightNum ( rindex ) 
      *                           NOT EQUAL LeftNum ( counter )
      *               ADD LeftNum ( counter ) TO SimScore
      *               SET rindex UP BY 1
      *              END-PERFORM
      *              SET rindex TO 1
           END-PERFORM

           MOVE SumOfDistances TO Sum-Result
           MOVE SimScore TO SimScore-Result
           STRING "The total sum of distances is "
                  Sum-Result
                  ", the SimScore is "
                  SimScore-Result
                  "."
             DELIMITED BY SIZE
             INTO Output-Msg
           END-STRING
           DISPLAY Output-Msg

           GOBACK
           .
      /
       END PROGRAM D01HISHY.
      /
