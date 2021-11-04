with Ada.Text_IO;

with WL.Binary_IO;

with Pdp11.Tokens;                   use Pdp11.Tokens;
with Pdp11.Lexical;                  use Pdp11.Lexical;

with Pdp11.Conversions;

with Pdp11.Assembler.Parser;

package body Pdp11.Assembler is

   ------------
   -- Append --
   ------------

   procedure Append
     (Assembly : in out Assembly_Type'Class;
      Line     : String)
   is
   begin
      Assembly.Lines.Append (Line);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Assembly : in out Assembly_Type'Class;
      Value    : Word_16)
   is
   begin
      Assembly.Set (Assembly.Current, Value);
      Assembly.Current := Assembly.Current + 2;
   end Append;

   -----------------
   -- Append_Byte --
   -----------------

   procedure Append_Byte
     (Assembly : in out Assembly_Type'Class;
      Value    : Word_8)
   is
   begin
      Assembly.Set (Assembly.Current, Value);
      Assembly.Current := Assembly.Current + 1;
   end Append_Byte;

   ------------
   -- Define --
   ------------

   procedure Define_Constant
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Value    : Word_16)
   is
   begin
      if not Assembly.Name_Map.Contains (Name) then
         Assembly.Symbols.Append (Name);
         Assembly.Name_Map.Insert (Name, Assembly.Symbols.Last_Index);
      end if;

      if Assembly.Definitions.Contains (Name) then
         Error ("redefinition of " & Name);
         Error (Assembly.Definitions.Element (Name).Location,
                "original definition of " & Name);
      else
         Assembly.Definitions.Insert
           (Key      => Name,
            New_Item => Symbol_Record'
              (Defined    => True,
               Sym_Type   => Constant_Symbol,
               Location   => GCS.Positions.Get_Current_Position,
               Id         => Assembly.Name_Map (Name),
               Value      => Value,
               References => <>));
      end if;
   end Define_Constant;

   ------------------
   -- Define_Label --
   ------------------

   procedure Define_Label
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Value    : Word_16)
   is
   begin
      if not Assembly.Name_Map.Contains (Name) then
         Assembly.Symbols.Append (Name);
         Assembly.Name_Map.Insert (Name, Assembly.Symbols.Last_Index);
      end if;

      if Assembly.Definitions.Contains (Name) then
         if Assembly.Definitions.Element (Name).Defined then
            Error ("redefinition of " & Name);
            Error (Assembly.Definitions.Element (Name).Location,
                   "original definition of " & Name);
         else
            Assembly.Definitions (Name).Defined := True;
            Assembly.Definitions (Name).Value := Value;
            Assembly.Definitions (Name).Location :=
              GCS.Positions.Get_Current_Position;
         end if;
      else
         Assembly.Definitions.Insert
           (Key      => Name,
            New_Item => Symbol_Record'
              (Defined    => True,
               Sym_Type   => Label_Symbol,
               Location   => GCS.Positions.Get_Current_Position,
               Id         => Assembly.Name_Map (Name),
               Value      => Value,
               References => <>));
      end if;
   end Define_Label;

   -----------
   -- Label --
   -----------

   procedure Label
     (Assembly : in out Assembly_Type'Class;
      Name     : String)
   is
   begin
      Assembly.Define_Label (Name, Assembly.Current);
      Assembly.Temp_Lbl_Start := Assembly.Current;
   end Label;

   -----------
   -- Label --
   -----------

   procedure Label
     (Assembly : in out Assembly_Type'Class;
      Index    : Natural)
   is
      Name : constant String :=
               Index'Image & Assembly.Temp_Lbl_Start'Image;
   begin
      Assembly.Define_Label (Name, Assembly.Current);
   end Label;

   ----------
   -- Link --
   ----------

   procedure Link (Assembly : in out Assembly_Type'Class) is
   begin
      for Sym of Assembly.Definitions loop
         if not Sym.Defined then
            Error (Sym.References.First_Element.Location,
                   "undefined: " & Assembly.Symbols (Sym.Id));
         else
            for Ref of Sym.References loop
               case Ref.Reference is
                  when Absolute =>
                     Assembly.Set (Ref.Address, Sym.Value);
                  when Relative =>
                     Assembly.Set (Ref.Address,
                                   Sym.Value - Ref.Address - 2);
                  when Branch =>
                     declare
                        Offset : constant Integer_16 :=
                                   (if Sym.Value >= Ref.Address + 2
                                    then Integer_16
                                      (Sym.Value - Ref.Address - 2)
                                    else -Integer_16
                                      (Ref.Address + 2 - Sym.Value))
                                   / 2;
                     begin
                        if Offset not in -128 .. 127 then
                           Error (Ref.Location, "branch too far");
                        end if;
                        Assembly.Set
                          (Ref.Address,
                           Word_8
                             (Pdp11.Conversions.As_Word_16 (Offset)
                              mod 256));
                     end;
               end case;
            end loop;
         end if;
      end loop;
   end Link;

   ----------
   -- List --
   ----------

   procedure List
     (Assembly : in out Assembly_Type'Class;
      Path     : String)
   is
      use Ada.Text_IO;
      File : Ada.Text_IO.File_Type;
   begin
      Create (File, Out_File, Path);
      for Line of Assembly.Lines loop
         Put_Line (File, Line);
      end loop;
      Close (File);
   end List;

   ----------
   -- Load --
   ----------

   procedure Load
     (Assembly : in out Assembly_Type'Class;
      Path     : String)
   is
   begin
      Open (Path);
      while Tok /= Tok_End_Of_File loop
         Pdp11.Assembler.Parser.Parse_Line (Assembly);
      end loop;
      Close;
   end Load;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Ref      : Reference_Type)
   is
      Address : constant Word_16 :=
                  (if Ref = Branch
                   then Assembly.Current - 2
                   else Assembly.Current);
   begin
      if not Assembly.Name_Map.Contains (Name) then
         Assembly.Symbols.Append (Name);
         Assembly.Name_Map.Insert (Name, Assembly.Symbols.Last_Index);
      end if;

      if not Assembly.Definitions.Contains (Name) then
         Assembly.Definitions.Insert
           (Key      => Name,
            New_Item => Symbol_Record'
              (Defined    => False,
               Sym_Type   => Label_Symbol,
               Location   => GCS.Positions.Get_Current_Position,
               Id         => Assembly.Name_Map (Name),
               Value      => 0,
               References => <>));
      end if;

      Assembly.Definitions (Name).References.Append
        (Symbol_Reference'
           (GCS.Positions.Get_Current_Position, Address, Ref));

   end Reference;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Assembly : in out Assembly_Type'Class;
      Label    : Natural;
      Ref      : Reference_Type)
   is
      Name : constant String :=
               Label'Image & Assembly.Temp_Lbl_Start'Image;
   begin
      Assembly.Reference (Name, Ref);
   end Reference;

   ----------
   -- Save --
   ----------

   procedure Save (Assembly : in out Assembly_Type'Class; Path : String) is
      use WL.Binary_IO;
      File : File_Type;
      Data_Size : constant Word_16 := Assembly.High - Assembly.Low + 1;
      Full_Size : constant Word_16 :=
        (if Data_Size mod 64 = 0 then Data_Size
         else (Data_Size / 64 + 1) * 64);
      Padding   : constant Word_16 := Full_Size - Data_Size;
   begin
      Create (File, Out_File, Path);
      for Address in Assembly.Low .. Assembly.High loop
         Write (File, WL.Binary_IO.Word_8 (Assembly.Memory (Address)));
      end loop;
      for I in 1 .. Padding loop
         Write (File, WL.Binary_IO.Word_8'(0));
      end loop;
      Close (File);
   end Save;

   ---------
   -- Set --
   ---------

   procedure Set
     (Assembly : in out Assembly_Type'Class;
      Address  : Word_16;
      Value    : Word_8)
   is
   begin
      Assembly.Memory (Address) := Value;
      Assembly.Low := Word_16'Min (Assembly.Low, Address);
      Assembly.High := Word_16'Max (Assembly.High, Address);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Assembly : in out Assembly_Type'Class;
      Address  : Word_16;
      Value    : Word_16)
   is
   begin
      Assembly.Set (Address, Word_8 (Value mod 256));
      Assembly.Set (Address + 1, Word_8 (Value / 256));
   end Set;

end Pdp11.Assembler;
