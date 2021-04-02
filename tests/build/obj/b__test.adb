pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E072 : Short_Integer; pragma Import (Ada, E072, "system__os_lib_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "ada__exceptions_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E052 : Short_Integer; pragma Import (Ada, E052, "ada__strings_E");
   E054 : Short_Integer; pragma Import (Ada, E054, "ada__strings__maps_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__strings__maps__constants_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "interfaces__c_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__object_reader_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "system__dwarf_lines_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__soft_links__initialize_E");
   E039 : Short_Integer; pragma Import (Ada, E039, "system__traceback__symbolic_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__tags_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ada__streams_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "gnat_E");
   E162 : Short_Integer; pragma Import (Ada, E162, "interfaces__c__strings_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "system__file_control_block_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "system__finalization_root_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ada__finalization_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__file_io_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "system__storage_pools_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "system__finalization_masters_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "system__storage_pools__subpools_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "ada__strings__unbounded_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "system__task_info_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "ada__calendar_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "ada__calendar__delays_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "ada__calendar__time_zones_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "ada__real_time_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "ada__text_io_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "system__direct_io_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "system__pool_global_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "system__pool_size_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "gnat__sockets_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "gnat__sockets__thin_common_E");
   E160 : Short_Integer; pragma Import (Ada, E160, "gnat__sockets__thin_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "system__regexp_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "ada__directories_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "system__tasking__initialization_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "system__tasking__protected_objects_E");
   E269 : Short_Integer; pragma Import (Ada, E269, "system__tasking__protected_objects__entries_E");
   E267 : Short_Integer; pragma Import (Ada, E267, "system__tasking__queuing_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "system__tasking__stages_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ib_ada_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "ib_ada__communication_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "ib_ada__communication__incomming_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "ib_ada__communication__outgoing_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "ib_ada__conn_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E254 := E254 - 1;
      E208 := E208 - 1;
      E212 := E212 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ib_ada__communication__outgoing__finalize_spec");
      begin
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ib_ada__communication__incomming__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ib_ada__communication__finalize_spec");
      begin
         F3;
      end;
      E115 := E115 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ib_ada__finalize_spec");
      begin
         F4;
      end;
      E269 := E269 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F5;
      end;
      E214 := E214 - 1;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__directories__finalize_spec");
      begin
         F6;
      end;
      E231 := E231 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__regexp__finalize_spec");
      begin
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gnat__sockets__finalize_body");
      begin
         E157 := E157 - 1;
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "gnat__sockets__finalize_spec");
      begin
         F9;
      end;
      E172 := E172 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__pool_size__finalize_spec");
      begin
         F10;
      end;
      E168 := E168 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__pool_global__finalize_spec");
      begin
         F11;
      end;
      E237 := E237 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__direct_io__finalize_spec");
      begin
         F12;
      end;
      E103 := E103 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "ada__text_io__finalize_spec");
      begin
         F13;
      end;
      E138 := E138 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__strings__unbounded__finalize_spec");
      begin
         F14;
      end;
      E119 := E119 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "system__storage_pools__subpools__finalize_spec");
      begin
         F15;
      end;
      E121 := E121 - 1;
      declare
         procedure F16;
         pragma Import (Ada, F16, "system__finalization_masters__finalize_spec");
      begin
         F16;
      end;
      declare
         procedure F17;
         pragma Import (Ada, F17, "system__file_io__finalize_body");
      begin
         E109 := E109 - 1;
         F17;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           True, False, False, True, True, True, True, False, 
           False, True, False, False, True, True, False, True, 
           True, False, True, True, True, True, False, False, 
           False, False, False, True, False, False, True, False, 
           True, False, True, True, False, True, False, True, 
           True, False, False, True, False, True, False, False, 
           False, False, False, True, True, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           False, False, False, False, False, False, True, True, 
           True, False, True, False),
         Count => (0, 0, 0, 0, 4, 3, 1, 0, 2, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      Ada.Containers'Elab_Spec;
      E040 := E040 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E052 := E052 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E054 := E054 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E058 := E058 + 1;
      Interfaces.C'Elab_Spec;
      E078 := E078 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Object_Reader'Elab_Spec;
      E080 := E080 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E047 := E047 + 1;
      System.Os_Lib'Elab_Body;
      E072 := E072 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E021 := E021 + 1;
      E013 := E013 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E039 := E039 + 1;
      E008 := E008 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E006 := E006 + 1;
      Ada.Streams'Elab_Spec;
      E105 := E105 + 1;
      Gnat'Elab_Spec;
      E155 := E155 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E162 := E162 + 1;
      System.File_Control_Block'Elab_Spec;
      E113 := E113 + 1;
      System.Finalization_Root'Elab_Spec;
      E112 := E112 + 1;
      Ada.Finalization'Elab_Spec;
      E110 := E110 + 1;
      System.File_Io'Elab_Body;
      E109 := E109 + 1;
      System.Storage_Pools'Elab_Spec;
      E125 := E125 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E121 := E121 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E119 := E119 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E138 := E138 + 1;
      System.Task_Info'Elab_Spec;
      E198 := E198 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E210 := E210 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E252 := E252 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E218 := E218 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E279 := E279 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E103 := E103 + 1;
      System.Direct_Io'Elab_Spec;
      E237 := E237 + 1;
      System.Pool_Global'Elab_Spec;
      E168 := E168 + 1;
      System.Pool_Size'Elab_Spec;
      E172 := E172 + 1;
      Gnat.Sockets'Elab_Spec;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E164 := E164 + 1;
      Gnat.Sockets.Thin'Elab_Body;
      E160 := E160 + 1;
      Gnat.Sockets'Elab_Body;
      E157 := E157 + 1;
      System.Regexp'Elab_Spec;
      E231 := E231 + 1;
      Ada.Directories'Elab_Spec;
      Ada.Directories'Elab_Body;
      E214 := E214 + 1;
      System.Tasking.Initialization'Elab_Body;
      E263 := E263 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E244 := E244 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E269 := E269 + 1;
      System.Tasking.Queuing'Elab_Body;
      E267 := E267 + 1;
      System.Tasking.Stages'Elab_Body;
      E259 := E259 + 1;
      ib_ada'elab_spec;
      E115 := E115 + 1;
      ib_ada.communication'elab_spec;
      ib_ada.communication.incomming'elab_spec;
      ib_ada.communication.outgoing'elab_spec;
      E212 := E212 + 1;
      ib_ada.conn'elab_spec;
      E208 := E208 + 1;
      ib_ada.communication.incomming'elab_body;
      E254 := E254 + 1;
      ib_ada.conn'elab_body;
      E250 := E250 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\olivier\Desktop\github_tests\ib_ada\tests\build\obj\test.o
   --   -LC:\Users\olivier\Desktop\github_tests\ib_ada\tests\build\obj\
   --   -LC:\Users\olivier\Desktop\github_tests\ib_ada\tests\build\obj\
   --   -LC:\Users\olivier\Desktop\github_tests\ib_ada\build\lib\
   --   -LC:\Users\olivier\Desktop\JEWL\source\
   --   -LC:/gnat/2020/lib/gcc/x86_64-pc-mingw32/9.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lws2_32
   --   -Xlinker
   --   --stack=0x200000,0x1000
   --   -mthreads
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
