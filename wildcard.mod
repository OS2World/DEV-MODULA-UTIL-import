IMPLEMENTATION MODULE WildCard;

        (********************************************************)
        (*                                                      *)
        (*            String matching with wildcards            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 June 1999                     *)
        (*  Last edited:        21 June 1999                    *)
        (*  Status:             Apparently working              *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)
(*                         SUBSTRING MATCHING                           *)
(************************************************************************)

PROCEDURE SubstringMatch (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL): BOOLEAN;             FORWARD;

(************************************************************************)

PROCEDURE HeadMatch (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL): BOOLEAN;

    (* Returns TRUE if any leading substring of input[j1..k1]           *)
    (* matches template[j2..k2].  We allow the empty string and the     *)
    (* whole string as special cases of "leading substring".            *)

    BEGIN
        LOOP
            (* From the left, input[j1] and template[j2] are the        *)
            (* first characters we haven't yet tested for a match.      *)

            IF j2 > k2 THEN

                (* No more template left; match by definition.  *)

                RETURN TRUE;

            ELSIF template[j2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not '*', so we have a definite mismatch.    *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?')
                    AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j1);  INC (j2);

        END (*LOOP*);

        (* If we reach here, template[j2] = '*'. *)

        REPEAT
            INC (j2);
        UNTIL (j2 > k2) OR (template[j2] <> '*');

        RETURN SubstringMatch (input, j1, k1, template, j2, k2);

    END HeadMatch;

(************************************************************************)

PROCEDURE SubstringMatch (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL): BOOLEAN;

    (* Returns TRUE if any contiguous substring of input[j1..k1]        *)
    (* matches template[j2..k2].                                        *)

    VAR j: CARDINAL;

    BEGIN
        j := j1;
        LOOP
            IF HeadMatch (input, j, k1, template, j2, k2) THEN
                RETURN TRUE;
            ELSIF j >= k1 THEN
                RETURN FALSE;
            END (*IF*);
            INC(j);
        END (*LOOP*);
    END SubstringMatch;

(************************************************************************)
(*                   THE EXTERNALLY CALLABLE PROCEDURE                  *)
(************************************************************************)

PROCEDURE WildMatch (VAR (*IN*) input, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if template and input are equal, with the extra     *)
    (* rules:                                                           *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)

    VAR j1, k1, j2, k2: CARDINAL;

    BEGIN
        j1 := 0;  k1 := LENGTH (input);
        j2 := 0;  k2 := LENGTH (template);

        IF k1 = 0 THEN

            (* Empty input; the only thing that can match is an *)
            (* empty or all-star template.                      *)

            LOOP
                IF j2 = k2 THEN RETURN TRUE
                ELSIF template[j2] = '*' THEN INC(j2)
                ELSE RETURN FALSE
                END (*IF*);
            END (*LOOP*);

        ELSIF k2 = 0 THEN

            (* Empty template, non-empty input. *)

            RETURN FALSE;

        END (*IF*);

        DEC (k1);  DEC(k2);

        (* Having disposed of the "empty" cases, we're now comparing    *)
        (* input[j1..k1] with template[j2..k2].                         *)

        LOOP
            (* From the left, input[j1] and template[j2] are the        *)
            (* first characters we haven't yet tested for a match.      *)

            IF j2 > k2 THEN

                (* No more template left; match iff we've also  *)
                (* exhausted the input.                         *)

                RETURN j1 > k1;

            ELSIF template[j2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not '*', so we have a definite mismatch.    *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?') AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j1);  INC (j2);

        END (*LOOP*);

        (* If we reach here, template[j2] = '*'. *)

        LOOP
            (* From the right, input[k1] and template[k2] are the first *)
            (* characters we haven't yet checked for a match.           *)

            IF template[k2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF k1 < j1 THEN

                (* Input exhausted, last unmatched template char *)
                (* is not '*', so we have a definite mismatch.   *)

                RETURN FALSE;

            ELSIF (template[k2] <> '?')
                      AND (CAP(input[k1]) <> CAP(template[k2])) THEN

                RETURN FALSE;

            END (*IF*);

            (* Special case: if k1=0 then we have to record that we've  *)
            (* exhausted the input without decrementing k1.  The same   *)
            (* problem doesn't arise for k2 because at this point we    *)
            (* know that we'll hit a '*' before exhausting the template.*)

            IF k1 = 0 THEN INC(j1) ELSE DEC (k1) END(*IF*);
            DEC (k2);

        END (*LOOP*);

        (* If we reach here, k2 >= j2, template[j2] = '*', and          *)
        (* template[k2] = '*'.  If we have several '*'s in a row, here  *)
        (* is where we reduce them down.                                *)

        REPEAT
            INC (j2);
        UNTIL (j2 > k2) OR (template[j2] <> '*');
        WHILE (j2 <= k2) AND (template[k2] = '*') DO
            DEC (k2);
        END (*WHILE*);

        RETURN SubstringMatch (input, j1, k1, template, j2, k2);

    END WildMatch;

(************************************************************************)

END WildCard.

