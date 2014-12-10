MODULE Imports;

        (********************************************************)
        (*                                                      *)
        (*       Discovering all modules used in a program      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            13 January 2000                 *)
        (*  Last edited:        15 January 2000                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT TextIO, Strings, IOChan, IOConsts, ChanConsts, SeqFile;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM Files IMPORT
    (* type *)  FilenameString,
    (* proc *)  StripSpaces, LocateModule;

FROM Scanner IMPORT
    (* proc *)  StartScan, Scan;

(********************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    InfoIndex = [0..2047];
    ExtendedInfoIndex = [0..MAX(InfoIndex)+1];

    (* The fields in a ModuleData record are:                           *)
    (*     name         the name of the module                          *)
    (*     filename     the name in the file system                     *)
    (*     IsDefinition TRUE iff this is a definition module            *)
    (*     IsSource     TRUE iff filename is the name of a source file  *)
    (*                                                                  *)
    (* For each module except the main module we actually have two      *)
    (* records, one for the definition module and one for the           *)
    (* implementation module.                                           *)

    ModuleData = RECORD
                     name, filename: FilenameString;
                     IsDefinition, IsSource: BOOLEAN;
                 END (*RECORD*);

    Option = (WriteModuleNames);
    OptionSet = SET OF Option;

VAR
    ModuleInfo: ARRAY InfoIndex OF ModuleData;

    (* ModuleInfo[NextFree] is the first unused array element. *)

    NextFree: ExtendedInfoIndex;

    Options: OptionSet;

(********************************************************************************)
(*                        PICKING UP PROGRAM ARGUMENTS                          *)
(********************************************************************************)

PROCEDURE GetParameter (VAR (*OUT*) result: FilenameString);

    (* Picks up an optional program argument from the command line. *)

    VAR args: ChanId;
        ParameterString: ARRAY [0..255] OF CHAR;

    BEGIN
        result := "";
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, ParameterString);
            Strings.Assign (ParameterString, result);
            StripSpaces (result);
        END (*IF*);
    END GetParameter;

(************************************************************************)
(*                  WORKING OUT THE MODULE DEPENDENCIES                 *)
(************************************************************************)

PROCEDURE AddModule (name: FilenameString);

    (* Puts a new module into the ModuleInfo array, unless it's         *)
    (* already there.                                                   *)

    VAR pos: ExtendedInfoIndex;

    BEGIN
        pos := 0;
        WHILE (pos < NextFree)
                     AND NOT Strings.Equal(ModuleInfo[pos].name, name) DO
            INC (pos);
        END (*WHILE*);
        IF (pos = NextFree) AND (pos < MAX(InfoIndex)) THEN
            ModuleInfo[pos].name := name;
            ModuleInfo[pos].IsDefinition := TRUE;
            INC (pos);
            ModuleInfo[pos].name := name;
            ModuleInfo[pos].IsDefinition := FALSE;
            INC (pos);
            NextFree := pos;
        END (*IF*);
    END AddModule;

(************************************************************************)

PROCEDURE FindImportsBy (j: InfoIndex);

    (* Reads the IMPORT lines in ModuleInfo[j].filename, adds new       *)
    (* entries to ModuleInfo if appropriate.                            *)

    VAR cid: IOChan.ChanId;  res: ChanConsts.OpenResults;
        token: FilenameString;
        alpha: BOOLEAN;

    (********************************************************************)

    PROCEDURE SkipToSemicolon;

        (* Scans forward until token is ';' or end of file reached. *)

        VAR ch: CHAR;

        BEGIN
            REPEAT
                Scan (token, alpha);
                ch := token[0];
            UNTIL (NOT alpha) AND ((ch = ';') OR (ch = Nul));
        END SkipToSemicolon;

    (********************************************************************)

    BEGIN
        SeqFile.OpenRead (cid, ModuleInfo[j].filename, SeqFile.text, res);
        IF res = ChanConsts.opened THEN
            StartScan (cid);

            (* Scan past the module header line. *)

            SkipToSemicolon;

            (* Search for IMPORT or FROM lines. *)

            LOOP
                Scan (token, alpha);
                IF alpha THEN
                    IF Strings.Equal (token, "IMPORT") THEN

                        (* Handle IMPORT x, y, z, ... *)

                        LOOP
                            Scan (token, alpha);
                            IF alpha THEN
                                AddModule (token);
                            ELSE
                                EXIT (*LOOP*);
                            END (*IF*);
                            Scan (token, alpha);
                            (* We're expecting a comma at this point. *)

                            IF alpha OR (token[0] <> ',') THEN
                                EXIT (*LOOP*);
                            END (*IF*);
                        END (*LOOP*);

                    ELSIF Strings.Equal (token, "FROM") THEN

                        (* Handle FROM x IMPORT ... *)

                        Scan (token, alpha);
                        IF alpha THEN
                            AddModule (token);
                            SkipToSemicolon;
                        ELSE
                            EXIT (*LOOP*);
                        END (*IF*);

                    ELSE
                        EXIT (*LOOP*);
                    END (*IF*);

                ELSIF token[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);

            END (*LOOP*);

            SeqFile.Close (cid);

        END (*IF*);

    END FindImportsBy;

(************************************************************************)

PROCEDURE Expand (j: InfoIndex);

    (* Assuming ModuleInfo[j].name is already set, i.e. the module      *)
    (* name is known: finds the corresponding file name, and updates    *)
    (* ModuleInfo to include modules imported by this one.              *)

    BEGIN
        WITH ModuleInfo[j] DO
            LocateModule (name, IsDefinition, filename, IsSource);
        END (*WITH*);
        IF ModuleInfo[j].IsSource THEN
            FindImportsBy (j);
        END (*IF*);
    END Expand;

(************************************************************************)

PROCEDURE FindAllImports;

    VAR j: InfoIndex;

    BEGIN
        NextFree := 0;
        IF ModuleInfo[0].name[0] <> Nul THEN
            ModuleInfo[0].IsDefinition := FALSE;
            NextFree := 1;
        END (*IF*);

        j := 0;
        WHILE j < NextFree DO
            Expand (j);
            INC (j);
        END (*WHILE*);
    END FindAllImports;

(************************************************************************)
(*                       WRITING THE RESULTS                            *)
(************************************************************************)

PROCEDURE WriteResults;

    (* Writes a list of file names to standard output. *)

    VAR j: InfoIndex;

    BEGIN
        IF NextFree = 0 THEN
            WriteString ("No modules");  WriteLn;
        ELSE
            FOR j := 0 TO NextFree-1 DO
                IF WriteModuleNames IN Options THEN
                    WriteString (ModuleInfo[j].name);
                    WriteString ("=");
                END (*IF*);
                IF (WriteModuleNames IN Options)
                           OR (ModuleInfo[j].filename[0] <> Nul) THEN
                    WriteString (ModuleInfo[j].filename);
                    WriteLn;
                END (*IF*);
            END (*FOR*);
        END (*IF*);
    END WriteResults;

(************************************************************************)
(*                           MAIN PROGRAM                               *)
(************************************************************************)

BEGIN
    Options := OptionSet {};
    GetParameter (ModuleInfo[0].name);
    FindAllImports;
    WriteResults;
END Imports.

