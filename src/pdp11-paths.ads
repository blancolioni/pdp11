package Pdp11.Paths is

   Config_Path : constant String :=
     "E:\git\pdp11\share\pdp11";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Pdp11.Paths;
