private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Vectors;
private with WL.String_Maps;
private with GCS.Positions;

package Pdp11.Assembler is

   type Assembly_Type is tagged private;

   procedure Load
     (Assembly : in out Assembly_Type'Class;
      Path     : String);

   procedure Link
     (Assembly : in out Assembly_Type'Class);

   procedure Save
     (Assembly : in out Assembly_Type'Class;
      Path     : String);

   procedure List
     (Assembly : in out Assembly_Type'Class;
      Path     : String);

   procedure Append
     (Assembly : in out Assembly_Type'Class;
      Line     : String);

private

   type Memory_Image is array (Word_16) of Word_8;

   type Symbol_Id is new Positive;

   package Symbol_Names is
     new Ada.Containers.Indefinite_Vectors
       (Symbol_Id, String);

   package Symbol_Ids is
     new WL.String_Maps (Symbol_Id);

   type Reference_Type is (Absolute, Relative, Branch);

   type Symbol_Reference is
      record
         Location  : GCS.Positions.File_Position;
         Address   : Word_16;
         Reference : Reference_Type;
      end record;

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Symbol_Reference);

   type Symbol_Type is (Constant_Symbol, Label_Symbol);

   type Symbol_Record is
      record
         Defined    : Boolean := False;
         Sym_Type   : Symbol_Type := Constant_Symbol;
         Location   : GCS.Positions.File_Position;
         Id         : Symbol_Id;
         Value      : Word_16 := 0;
         References : Reference_Lists.List;
      end record;

   package Symbol_Maps is
     new WL.String_Maps (Symbol_Record);

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Assembly_Type is tagged
      record
         Memory         : Memory_Image := (others => 0);
         Low            : Word_16 := Word_16'Last;
         High           : Word_16 := Word_16'First;
         Current        : Word_16 := 0;
         Temp_Lbl_Start : Word_16 := 0;
         Symbols        : Symbol_Names.Vector;
         Name_Map       : Symbol_Ids.Map;
         Definitions    : Symbol_Maps.Map;
         Lines          : String_Lists.List;
      end record;

   procedure Append
     (Assembly : in out Assembly_Type'Class;
      Value    : Word_16);

   procedure Append_Byte
     (Assembly : in out Assembly_Type'Class;
      Value    : Word_8);

   procedure Set
     (Assembly : in out Assembly_Type'Class;
      Address  : Word_16;
      Value    : Word_8);

   procedure Set
     (Assembly : in out Assembly_Type'Class;
      Address  : Word_16;
      Value    : Word_16);

   procedure Define_Constant
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Value    : Word_16);

   procedure Define_Label
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Value    : Word_16);

   procedure Label
     (Assembly : in out Assembly_Type'Class;
      Name     : String);

   procedure Label
     (Assembly : in out Assembly_Type'Class;
      Index    : Natural);

   procedure Reference
     (Assembly : in out Assembly_Type'Class;
      Name     : String;
      Ref      : Reference_Type);

   procedure Reference
     (Assembly : in out Assembly_Type'Class;
      Label    : Natural;
      Ref      : Reference_Type);

end Pdp11.Assembler;
