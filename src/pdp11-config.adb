with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with WL.String_Maps;

package body Pdp11.Config is

   package Configuration_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Configuration_Maps is
     new WL.String_Maps (Configuration_Lists.List, Configuration_Lists."=");

   Configuration : Configuration_Maps.Map;

   ----------------
   -- Get_Config --
   ----------------

   function Get_Config
     (Section_Name  : String;
      Default_Value : String := "")
      return String
   is
   begin
      if Configuration.Contains (Section_Name) then
         return Configuration (Section_Name).First_Element;
      else
         return Default_Value;
      end if;
   end Get_Config;

   ----------------
   -- Get_Config --
   ----------------

   function Get_Config
     (Section_Name  : String;
      Default_Value : Word_16 := 0)
      return Word_16
   is
      Image : constant String := Get_Config (Section_Name, "");
   begin
      if Image = "" then
         return Default_Value;
      else
         return Word_16'Value ("8#" & Image & "#");
      end if;
   end Get_Config;

   ---------------------
   -- Iterate_Devices --
   ---------------------

   procedure Iterate_Config
     (Section_Name : String;
      Process      : not null access
        procedure (Value : String))
   is
   begin
      for Element of Configuration.Element (Section_Name) loop
         declare
            --  use Ada.Strings, Ada.Strings.Fixed;
            --  Space : constant Natural := Index (Element, " ");
            --  Name  : constant String :=
            --            Trim
            --        ((if Space = 0 then Element else Element (1 .. Space)),
            --               Both);
            --  Args  : constant String :=
            --            Trim
            --              ((if Space = 0 then ""
            --               else Element (Space .. Element'Last)),
            --               Both);
         begin
            Process (Element);
         end;
      end loop;
   end Iterate_Config;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration (Path : String) is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;
      File : File_Type;
   begin
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Equal_Sign : constant Natural := Index (Line, "=");
            Section    : constant String :=
                           Trim
                             ((if Equal_Sign = 0 then Line
                              else Line (1 .. Equal_Sign - 1)),
                              Ada.Strings.Both);
            Value      : constant String :=
                           Trim
                             ((if Equal_Sign = 0 then ""
                              else Line (Equal_Sign + 1 .. Line'Last)),
                              Ada.Strings.Both);
         begin
            if not Configuration.Contains (Section) then
               Configuration.Insert (Section, Configuration_Lists.Empty_List);
            end if;
            Configuration (Section).Append (Value);
         end;
      end loop;
   end Load_Configuration;

end Pdp11.Config;
