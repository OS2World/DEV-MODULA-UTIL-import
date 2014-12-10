IMPLEMENTATION MODULE Files;

        (********************************************************)
        (*                                                      *)
        (*                  File operations                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 January 2000                 *)
        (*  Last edited:        15 January 2000                 *)
        (*  Status:             Done                            *)
        (*                                                      *)
        (*     WARNING: This module contains some code that     *)
        (*     is specific for XDS Modula-2 on the OS/2         *)
        (*     operating system.  Adjust as appropriate when    *)
        (*     porting this to another system.                  *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, OS2, IOChan, IOConsts, ChanConsts, SeqFile, TextIO;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

(************************************************************************)

CONST
    (* NameSize gives the maximum number of characters from the module  *)
    (* name used to form the corresponding file name.  If the value is  *)
    (* zero then there is no limit.  For XDS, set the value to 8 if     *)
    (* you're generating DOS-compatible 8.3 file names, and to 0        *)
    (* otherwise.                                                       *)

    NameSize = 8;

    Nul = CHR(0);
    TableSize = 2048;

TYPE
    PathString = ARRAY [0..2047] OF CHAR;

VAR
    (* The number of entries in Table. *)

    TCount: CARDINAL;

    (* Redirection table. *)

    Table: ARRAY [0..TableSize-1] OF
               RECORD
                   mask, result: PathString;
               END (*RECORD*);

(************************************************************************)
(*               MAPPING FROM MODULE NAME TO FILE NAME                  *)
(************************************************************************)

PROCEDURE LocateFile (VAR (*IN*) name: FilenameString;
                      VAR (*OUT*) fullname: FilenameString): BOOLEAN;

    (* Given a relative file name, expands it out to a fully qualified  *)
    (* file name, using the search paths as specified by the            *)
    (* redirection list.  The function result is TRUE iff the file was  *)
    (* found.                                                           *)

    VAR success: BOOLEAN;
        path: PathString;
        k: CARDINAL;

    BEGIN
        k := 0;
        path := ".";
        LOOP
            IF k >= TCount THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF WildMatch (name, Table[k].mask) THEN
                path := Table[k].result;
                EXIT (*LOOP*);
            END (*IF*);
            INC (k);
        END (*LOOP*);
        success := OS2.DosSearchPath( 0, path, name,
                                     fullname, SIZE(fullname) ) = 0;
        RETURN success;
    END LocateFile;

(************************************************************************)
(*               MAPPING FROM MODULE NAME TO FILE NAME                  *)
(************************************************************************)

PROCEDURE LocateModule (modulename: FilenameString;
                        definition: BOOLEAN;
                        VAR (*OUT*) filename: FilenameString;
                        VAR (*OUT*) IsSource: BOOLEAN);

    (* Given a module name, works out the full file name.  If parameter *)
    (* 'definition' is true then we are looking for a definition module.*)
    (* On return IsSource=TRUE iff this is a source file rather than    *)
    (* a binary file.                                                   *)

    VAR partname: FilenameString;

    BEGIN
        IF (NameSize > 0) AND (NameSize <= MAX(FilenameIndex)) THEN
            modulename[NameSize] := Nul;
        END (*IF*);
        IsSource := FALSE;
        Strings.Assign (modulename, partname);
        IF definition THEN
            Strings.Append (".DEF", partname);
            IF LocateFile (partname, filename) THEN
                IsSource := TRUE;
            ELSE
                Strings.Assign (modulename, partname);
                Strings.Append (".SYM", partname);
                IF LocateFile (partname, filename) THEN
                ELSE
                    filename := "";
                END (*IF*);
            END (*IF*);
        ELSE
            Strings.Append (".MOD", partname);
            IF LocateFile (partname, filename) THEN
                IsSource := TRUE;
            ELSE
                Strings.Assign (modulename, partname);
                Strings.Append (".OBJ", partname);
                IF LocateFile (partname, filename) THEN
                ELSE
                    filename := "";
                END (*IF*);
            END (*IF*);
        END (*IF*);
    END LocateModule;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

PROCEDURE StripSpaces (VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Removes leading and trailing spaces from string. *)

    VAR k: CARDINAL;

    BEGIN
        k := Strings.Length (string);
        IF k > 0 THEN
            WHILE string[k-1] = ' ' DO
                DEC (k);
            END (*WHILE*);
            string[k] := Nul;
        END (*IF*);
        k := 0;
        WHILE string[k] = ' ' DO
            INC (k);
        END (*WHILE*);
        IF k > 0 THEN
            Strings.Delete (string, 0, k);
        END (*IF*);
    END StripSpaces;

(************************************************************************)

PROCEDURE ReadPathStrings;

    CONST RedirectionFileName = "XC.RED";

    VAR cid: IOChan.ChanId;  res: ChanConsts.OpenResults;
        buffer: ARRAY [0..4095] OF CHAR;
        found: BOOLEAN;  pos: CARDINAL;

    BEGIN
        TCount := 0;
        SeqFile.OpenRead (cid, RedirectionFileName, SeqFile.text, res);
        IF res = ChanConsts.opened THEN
            LOOP
                IF TCount >= TableSize THEN
                    WriteString ("Redirection table is full");
                    WriteLn;
                    EXIT (*LOOP*);
                END (*IF*);
                TextIO.ReadRestLine (cid, buffer);
                IF IOChan.ReadResult(cid) <> IOConsts.allRight THEN
                    EXIT (*LOOP*);
                END (*IF*);
                TextIO.SkipLine (cid);
                Strings.FindNext ('=', buffer, 0, found, pos);
                IF found THEN
                    Strings.Assign (buffer, Table[TCount].result);
                    Strings.Delete (Table[TCount].result, 0, pos+1);
                    StripSpaces (Table[TCount].result);
                    buffer[pos] := Nul;
                    StripSpaces (buffer);
                    Strings.Assign (buffer, Table[TCount].mask);
                    INC (TCount);
                END (*IF*);
            END (*LOOP*);
            SeqFile.Close (cid);
        ELSE
            Table[0].mask := '*';  Table[0].result := '.';
            TCount := 0;
        END (*IF*);

    END ReadPathStrings;

(************************************************************************)

BEGIN
    ReadPathStrings;
END Files.

