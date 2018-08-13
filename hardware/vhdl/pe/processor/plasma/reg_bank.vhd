------------------------------------------------------------------------------------------------
--
--  DISTRIBUTED HEMPS  - version 5.0
--
--  Research group: GAPH-PUCRS    -    contact   fernando.moraes@pucrs.br
--
--  Distribution:  September 2013
--
--  Source name:  reg_bank.vhd
--
--  AUTHOR: Steve Rhoads (rhoadss@yahoo.com)
--
--  DATE CREATED: 2/2/01
--
--  COPYRIGHT: Software placed into the public domain by the author.
--    Software 'as is' without warranty.  Author liable for nothing.
--
--  Brief description:  Implements a register bank with 32 registers that are 32-bits wide.
--    			There are two read-ports and one write port.
--
------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use work.mlite_pack.all;

entity reg_bank is
   port(clk            : in  std_logic;
      reset_in       : in  std_logic;
      pause          : in  std_logic;
      rs_index       : in  std_logic_vector(5 downto 0);
      rt_index       : in  std_logic_vector(5 downto 0);
      rd_index       : in  std_logic_vector(5 downto 0);
      reg_source_out : out std_logic_vector(31 downto 0);
      reg_target_out : out std_logic_vector(31 downto 0);
      reg_dest_new   : in  std_logic_vector(31 downto 0); 
      page           : out std_logic_vector(31 downto 0);
      intr_enable    : out std_logic);
end; --entity reg_bank


--------------------------------------------------------------------
-- The ram_block architecture attempts to use TWO dual-port memories.
-- Different FPGAs and ASICs need different implementations.
-- Choose one of the RAM implementations below.
-- I need feedback on this section!
--------------------------------------------------------------------
architecture ram_block of reg_bank is

   signal intr_enable_reg : std_logic;
   type ram_type is array(31 downto 0) of std_logic_vector(31 downto 0);
   
   --controls access to dual-port memories
   signal addr_read1, addr_read2 : std_logic_vector(4 downto 0);
   signal addr_write             : std_logic_vector(4 downto 0);
   signal data_out1, data_out2   : std_logic_vector(31 downto 0);
   signal write_enable           : std_logic;  
   signal page_reg               : std_logic_vector(31 downto 0);

begin
  
reg_proc: process(clk, rs_index, rt_index, rd_index, reg_dest_new, 
      intr_enable_reg, data_out1, data_out2, reset_in, pause)
begin
   page <= page_reg;

   --setup for first dual-port memory
   if rs_index = "101110" then  --reg_epc CP0 14
      addr_read1 <= "00000";
   else
      addr_read1 <= rs_index(4 downto 0);
   end if;
   
   case rs_index is
   when "000000" => reg_source_out <= ZERO;
   when "101100" => reg_source_out <= ZERO(31 downto 1) & intr_enable_reg;
   
   --interrupt vector address = 0x44 (syscall)
   when "111110" => reg_source_out <= ZERO(31 downto 8) & "01000100";
                    page <= (others => '0');
                    
   --interrupt vector address = 0x3c (hardware)
   when "111111" => reg_source_out <= ZERO(31 downto 8) & "00111100";
                    page <= (others => '0');
                    
   when others   => reg_source_out <= data_out1;
   end case;

   --setup for second dual-port memory
   addr_read2 <= rt_index(4 downto 0);
   case rt_index is
   when "000000" => reg_target_out <= ZERO;
   when others   => reg_target_out <= data_out2;
   end case;

   --setup write port for both dual-port memories
   if rd_index /= "000000" and rd_index /= "101100" and rd_index /= "101010" and pause = '0' then
      write_enable <= '1';
   else
      write_enable <= '0';
   end if;
   
   if rd_index = "101110" then      --reg_epc CP0 14
      addr_write <= "00000";
   else
      addr_write <= rd_index(4 downto 0);
   end if;

   if reset_in = '1' then
      intr_enable_reg <= '0';
      page_reg <= (others=>'0');
   elsif rising_edge(clk) then
      
      --disable interrupts
      if rd_index = "101110" then   --reg_epc CP0 14
         intr_enable_reg <= '0';       
      
      --sets interrupts register
      elsif rd_index = "101100" then
         intr_enable_reg <= reg_dest_new(0);      
        
      --sets page register
      elsif rd_index = "101010" then
         page_reg <= reg_dest_new;
      end if;
      
      --resets page on interrupts    
      if rs_index = "111110" or  rs_index = "111111" then
        page_reg <= (others => '0');      
      end if;
      
   end if;

   intr_enable <= intr_enable_reg; 
   
end process;   

  ram_proc: process(clk, addr_read1, addr_read2, 
		addr_write, reg_dest_new, write_enable)
  variable tri_port_ram : ram_type;
  begin
	 data_out1 <= tri_port_ram(conv_integer(addr_read1));
	 data_out2 <= tri_port_ram(conv_integer(addr_read2));
	 if rising_edge(clk) then
		if write_enable = '1' then
		   tri_port_ram(conv_integer(addr_write)) := reg_dest_new;
		end if;
	 end if;
  end process;
 
end; --architecture ram_block
