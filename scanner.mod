IMPLEMENTATION MODULE Scanner;

        (********************************************************)
        (*                                                      *)
        (*              Simplified lexical analyser             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 January 2000                 *)
        (*  Last edited:        15 January 2000                 *)
        (*  Status:             Complete, untested              *)
        (*                                                      *)
        (*    Remark: For checking imports we need to parse     *)
        (*    only a small subset of the language, and one      *)
        (*    consequence of this is that we can get away       *)
        (*    with a very crude lexical analyser.  We need      *)
        (*    only detect whitespace, comments, alphanumeric    *)
        (*    strings starting with a letter; everything else   *)
        (*    is interpreted as a one-character token.          *)
        (*                                                      *)
        (********************************************************)

IMPORT TextIO, Strings, IOChan, IOConsts;

(********************************************************************************)

CONST
    CharsPerLine = 1024;
    Nul = CHR(0);  Tab = CHR(9);  CtrlZ = CHR(26);

TYPE
    LineIndex = [0..CharsPerLine-1];
    CharSet = SET OF CHAR;

VAR
    LineBuffer: ARRAY LineIndex OF CHAR;
    NextPos: CARDINAL;
    fileid: IOChan.ChanId;

CONST
    Letters = CharSet{'A'..'Z', 'a'..'z'};
    IdChars = Letters + CharSet {'0'..'9', '_', '$'};

(************************************************************************)

PROCEDURE StartScan (cid: IOChan.ChanId);

    (* Resets the scanner to work with a new file. *)

    BEGIN
        LineBuffer := "";  NextPos := 0;
        fileid := cid;
    END StartScan;

(************************************************************************)

PROCEDURE GetNextLine;

    (* Reads a new line into LineBuffer, resets NextPos. *)

    VAR status: IOConsts.ReadResults;

    BEGIN
        TextIO.ReadRestLine (fileid, LineBuffer);
        NextPos := 0;
        status := IOChan.ReadResult(fileid);
        IF (status <> IOConsts.allRight) AND (status <> IOConsts.endOfLine) THEN
            LineBuffer[0] := CtrlZ;
        END (*IF*);
        TextIO.SkipLine (fileid);
    END GetNextLine;

(************************************************************************)

PROCEDURE SkipComments;

    (* Skips over comments, including nested comments. *)

    VAR ch: CHAR;

    BEGIN
        INC (NextPos, 2);
        LOOP
            ch := LineBuffer[NextPos];  INC(NextPos);
            IF ch = Nul THEN
                GetNextLine;
                ch := LineBuffer[0];
                IF ch = CtrlZ THEN
                    EXIT (*LOOP*);
                END (*IF*);
                NextPos := 1;
            END (*IF*);
            IF (ch = '(') AND (LineBuffer[NextPos] = '*') THEN
                SkipComments;
            ELSIF (ch = '*') AND (LineBuffer[NextPos] = ')') THEN
                INC (NextPos);
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
    END SkipComments;

(************************************************************************)

PROCEDURE SkipBlanks;

    (* Skips over whitespace and comments. *)

    VAR ch: CHAR;

    BEGIN
        LOOP
            ch := LineBuffer[NextPos];
            WHILE ch = Nul DO
                GetNextLine;
                ch := LineBuffer[0];
            END (*WHILE*);
            IF ch = CtrlZ THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF (ch = ' ') OR (ch = Tab) THEN
                INC (NextPos);
            ELSIF (ch = '(') AND (LineBuffer[NextPos+1] = '*') THEN
                SkipComments;
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
    END SkipBlanks;

(************************************************************************)

PROCEDURE Scan (VAR (*OUT*) token: ARRAY OF CHAR;
                VAR (*OUT*) alphanumeric: BOOLEAN);

    (* Returns the next input token, returns alphanumeric=TRUE iff      *)
    (* this is an identifier or a keyword.                              *)

    VAR k: CARDINAL;

    BEGIN
        SkipBlanks;
        IF LineBuffer[NextPos] IN Letters THEN
            alphanumeric := TRUE;
            k := 0;
            REPEAT
                token[k] := LineBuffer[NextPos];
                INC (k);  INC (NextPos);
            UNTIL (k > HIGH(token)) OR NOT (LineBuffer[NextPos] IN IdChars);
            IF k <= HIGH(token) THEN
                token[k] := Nul;
            END (*IF*);
        ELSE
            alphanumeric := FALSE;
            token[0] := LineBuffer[NextPos];
            IF token[0] = CtrlZ THEN
                token[0] := Nul;
            ELSE
                INC (NextPos);
            END (*IF*);
            token[1] := Nul;
        END (*IF*);
    END Scan;

(************************************************************************)

END Scanner.

