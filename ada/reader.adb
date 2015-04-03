with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.Identifier;
with Opentoken.Recognizer.Integer;
with Opentoken.Recognizer.Keyword;
with Opentoken.Recognizer.Line_Comment;
with Opentoken.Recognizer.Real;
with Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.Single_Character_Set;
with Opentoken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with Opentoken.Token.Enumerated.Analyzer;
with Smart_Pointers;

package body Reader is

   package ACL renames Ada.Characters.Latin_1;

   type Lexemes is (Int, Float_Tok, Sym,
                    Nil, True_Tok, False_Tok, Exp_Tok, Splice_Unq,
                    Str, Atom,
                    Whitespace, Comment);

   package Lisp_Tokens is new Opentoken.Token.Enumerated (Lexemes);
   package Tokenizer is new Lisp_Tokens.Analyzer;

   Exp_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("**"));

   Splice_Unq_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("~@"));

   Nil_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("nil"));

   True_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("true"));

   False_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Keyword.Get ("false"));

   ID_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Identifier.Get);

   Int_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Integer.Get);

   Float_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Real.Get);

   -- Use the C style for escaped strings.
   String_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get
       (Opentoken.Recognizer.String.Get
         (Escapeable => True, 
          Double_Delimiter => False));

   -- Atom definition
   -- Note Start_Chars includes : for keywords.
   Start_Chars : Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps."or"
       (Ada.Strings.Maps.Constants.Letter_Set,
        Ada.Strings.Maps.To_Set (':'));

   Body_Chars : Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps."or"
       (Ada.Strings.Maps.Constants.Alphanumeric_Set,
        Ada.Strings.Maps.To_Set ("-!"));

   Atom_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get
       (Opentoken.Recognizer.Identifier.Get (Start_Chars, Body_Chars));

   Lisp_Syms : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ("[]{}()'`~^@+-*/");

   Sym_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Single_Character_Set.Get (Lisp_Syms));

   Lisp_Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (ACL.HT & ACL.Space & ACL.Comma);

   Whitesp_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Character_Set.Get (Lisp_Whitespace));

   Comment_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Line_Comment.Get (";"));

   Syntax : constant Tokenizer.Syntax :=
     (Int        => Int_Recognizer,
      Float_Tok  => Float_Recognizer,
      Sym        => Sym_Recognizer,
      Nil        => Nil_Recognizer,
      True_Tok   => True_Recognizer,
      False_Tok  => False_Recognizer,
      Exp_Tok    => Exp_Recognizer,
      Splice_Unq => Splice_Unq_Recognizer,
      Str        => String_Recognizer,
      Atom       => Atom_Recognizer,
      Whitespace => Whitesp_Recognizer,
      Comment    => Comment_Recognizer);

   Input_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   Analyzer : Tokenizer.Instance :=
     Tokenizer.Initialize (Syntax, Input_Feeder'access);


   -- This is raised if an invalid character is encountered
   Lexical_Error : exception;

   -- The unterminated string error
   String_Error : exception;


   function Get_Token_String return String is
   begin
      return Tokenizer.Lexeme (Analyzer);
   end Get_Token_String;


   function Get_Token_Char return Character is
      S : String := Tokenizer.Lexeme (Analyzer);
   begin
      return S (S'First);
   end Get_Token_Char;


   -- Saved_Line is needed to detect the unterminated string error.
   Saved_Line : String (1..Max_Line_Len);

   function Get_Token return Types.Mal_Handle is
      use Types;
      Res : Types.Mal_Handle;
   begin
      Tokenizer.Find_Next (Analyzer);
      case Tokenizer.ID (Analyzer) is
         when Int =>
            Res := New_Int_Mal_Type
              (Int => Mal_Integer'Value (Get_Token_String));
         when Float_Tok =>
            Res := New_Float_Mal_Type
              (Floating => Mal_Float'Value (Get_Token_String));
         when Sym =>
            Res := New_Sym_Mal_Type (Sym => Get_Token_Char);
         when Nil =>
            Res := New_Atom_Mal_Type (Str => Get_Token_String);
         when True_Tok =>
            Res := New_Atom_Mal_Type (Str => Get_Token_String);
         when False_Tok =>
            Res := New_Atom_Mal_Type (Str => Get_Token_String);
         when Exp_Tok =>
            Res := New_Atom_Mal_Type (Str => Get_Token_String);
         when Splice_Unq =>
            Res := New_Unitary_Mal_Type
              (Func => Splice_Unquote,
               Op => Smart_Pointers.Null_Smart_Pointer);
         when Str =>
            Res := New_String_Mal_Type (Str => Get_Token_String);
         when Atom =>
            Res := New_Atom_Mal_Type (Str => Get_Token_String);
         when Whitespace | Comment => null;
      end case;
      return Res;

   exception

      when E : OpenToken.Syntax_Error =>

-- Extra debug info
--         declare
--            Err_Pos : Integer := Analyzer.Column + 1;
--         begin
--            for J in 1..Err_Pos + 5 loop
--               Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ' ');
--            end loop;
--            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "^");
--         end;

--         Ada.Text_IO.Put_Line
--           (Ada.Text_IO.Standard_Error,
--            Ada.Exceptions.Exception_Information (E));

         declare
            Col : Integer := Analyzer.Column;
         begin
            if Saved_Line (Col) ='"' then
               raise String_Error;
            else
               raise Lexical_Error;
            end if;
         end;

   end Get_Token;


   -- Parsing
   function Read_Form return Types.Mal_Handle;

   function Read_List (LT : Types.List_Types)
   return Types.Mal_Handle is

      use Types;
      List_SP, MTA : Mal_Handle;
      List_P : List_Ptr;
      Close : Character := Types.Closing (LT);

   begin

      List_SP := New_List_Mal_Type (List_Type => LT);

      -- Need to append to a variable so...
      List_P := Deref_List (List_SP);

      loop
         MTA := Read_Form;
         exit when Is_Null (MTA) or else
                   (Deref (MTA).Sym_Type = Sym and then
                    Sym_Mal_Type (Deref (MTA).all).Symbol = Close);
         Append (List_P.all, MTA);
      end loop;
      return List_SP;
   exception
      when Lexical_Error =>

        -- List_MT about to go out of scope but its a Mal_Handle
        -- so it is automatically garbage collected.

        return New_Error_Mal_Type (Str => "expected '" & Close & "'");

   end Read_List;


   function Read_Form return Types.Mal_Handle is
      use Types;
      MTS : Mal_Handle;
      Symbol : Character;
   begin

      MTS := Get_Token;

      if Is_Null (MTS) then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;

      if Deref (MTS).Sym_Type = Sym then

         Symbol := Sym_Mal_Type (Deref (MTS).all).Symbol;
         -- Listy things and quoting...
         if Symbol = '(' then
            return Read_List (List_List);
         elsif Symbol = '[' then
            return Read_List (Vector_List);
         elsif Symbol = '{' then
            return Read_List (Hashed_List);
         elsif Symbol = '^' then
            declare
               Meta, Obj : Mal_Handle;
            begin
               Meta := Read_Form;
               Obj := Read_Form;
               declare
                  MT : Mal_Ptr := Deref (Obj);
               begin
                  Set_Meta (MT.all, Meta);
               end;
               return Obj;
            end;
         elsif Symbol = ACL.Apostrophe then
            return New_Unitary_Mal_Type (Func => Quote, Op => Read_Form);
         elsif Symbol = ACL.Grave then
            return New_Unitary_Mal_Type (Func => Quasiquote, Op => Read_Form);
         elsif Symbol = ACL.Tilde then
            return New_Unitary_Mal_Type (Func => Unquote, Op => Read_Form);
         elsif Symbol = ACL.Commercial_At then
            return New_Unitary_Mal_Type (Func => Deref, Op => Read_Form);
         else
            return MTS;
         end if;

      elsif Deref(MTS).Sym_Type = Unitary and then
            Unitary_Mal_Type (Deref (MTS).all).Get_Func = Splice_Unquote then

         return New_Unitary_Mal_Type (Func => Splice_Unquote, Op => Read_Form);

      else
         return MTS;
      end if;

   exception
      when String_Error =>
        return New_Error_Mal_Type (Str => "expected '""'");
   end Read_Form;


   function Read_Str (S : String) return Types.Mal_Handle is
      I, Str_Len : Natural := S'Length;
   begin
      -- Filter out lines consisting of only whitespace and/or comments
      I := 1;
      while I <= Str_Len and then
            Ada.Strings.Maps.Is_In (S (I), Lisp_Whitespace) loop
         I := I + 1;
      end loop;
      if I > Str_Len or else S (I) = ';' then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;
       
      Analyzer.Reset;
      Input_Feeder.Set (S);
      Saved_Line (1..S'Length) := S;  -- Needed for error recovery
      return Read_Form;
   end Read_Str;
   

end Reader;
