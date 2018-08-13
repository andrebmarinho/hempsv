library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
--use ieee.numeric_std.all;

entity clarvi_wrapper is
    generic (
        ADDR_WIDTH    : integer := 29;
		PAGE_NUMBER_H_INDEX: integer := 16;
		PAGE_SIZE_H_INDEX: integer := 15
    );
    port(
        clock        		: in  std_logic :='0';
       	reset               : in  std_logic :='0';
        cpu_number        	: in integer;
		trace				: in std_logic;
        data_address        : out std_logic_vector(ADDR_WIDTH+2 downto 0);
        wbe_data            : out std_logic_vector(3 downto 0);
        data_writedata      : out std_logic_vector(ADDR_WIDTH+2 downto 0);
        data_readdata   	: in std_logic_vector(ADDR_WIDTH+2 downto 0);
        instr_address       : out std_logic_vector(ADDR_WIDTH+2 downto 0);   
        instr_readdata      : in std_logic_vector(ADDR_WIDTH+2 downto 0);
        instr_writedata     : out std_logic_vector(ADDR_WIDTH+2 downto 0) := x"00000000";
        wbe_instr           : out std_logic_vector(3 downto 0) := "0000";
		page           		: in std_logic_vector(31 downto 0);
		cpu_mem_pause		: in std_logic;
        interrupt           : in std_logic;
		interrupt_ret		: out std_logic;
        ecall           	: out std_logic
	);
end entity clarvi_wrapper;

architecture wrapper of clarvi_wrapper is 	
	constant IMEM_BASE : natural := 0;
	constant IMEM_END  : natural := ADDR_WIDTH+2;
	constant ZERO          : std_logic_vector(31 downto 0) :=
        "00000000000000000000000000000000";

	-- Memorie interfaces:
	signal data_byteenable 		: std_logic_vector(3 downto 0);         -- 4b

	-- signal data_read 			: std_logic;
	signal data_write 			: std_logic;
	signal data_waitrequest 	: std_logic := '0';
	signal data_readdatavalid 	: std_logic;
	
	--signal instr_address : std_logic_vector(ADDR_WIDTH-1 downto 0); -- 29b
	signal instr_read 			: std_logic;
	signal instr_waitrequest 	: std_logic := '0';
	signal instr_readdatavalid 	: std_logic := '1';
		
	signal avs_a_write 			: std_logic;
	
	signal data_address_wop 	: std_logic_vector(ADDR_WIDTH-1 downto 0);
	signal instr_address_wop 	: std_logic_vector(ADDR_WIDTH-1 downto 0);
	signal data_address_full    : std_logic_vector(ADDR_WIDTH+2 downto 0);
	signal instr_address_full   : std_logic_vector(ADDR_WIDTH+2 downto 0);

	signal reset_cpu 			: std_logic;
	signal reset_reg      		: std_logic_vector(4 downto 0);

begin
    wbe_data 			<= data_byteenable 	when avs_a_write = '1' else "0000";
	wbe_instr 			<= "0000";
	avs_a_write 		<= data_write; 
	
	data_waitrequest <= cpu_mem_pause;

	-- data_address <= main_data_address;

	instr_address_full <= '0' & instr_address_wop & "00";
	data_address_full  <= '0' & data_address_wop & "00";

	instr_address <= instr_address_full(31 downto PAGE_NUMBER_H_INDEX+1) &
					 page(PAGE_NUMBER_H_INDEX downto PAGE_SIZE_H_INDEX+1) & 
					 instr_address_full(PAGE_SIZE_H_INDEX downto 2) & 
					 "00" when page /= ZERO else instr_address_full;
	
	data_address <= data_address_full(31 downto PAGE_NUMBER_H_INDEX+1) &
					page(PAGE_NUMBER_H_INDEX downto PAGE_SIZE_H_INDEX+1) & 
					data_address_full(PAGE_SIZE_H_INDEX downto 2) & 
					"00" when page /= ZERO else	data_address_full;



	-- instr_address_full <= "000" & 
	-- 				 instr_address_wop(ADDR_WIDTH-1 downto PAGE_NUMBER_H_INDEX+1) & 
	-- 				 page(PAGE_NUMBER_H_INDEX downto PAGE_SIZE_H_INDEX+1) & 
	-- 				 instr_address_wop(PAGE_SIZE_H_INDEX downto 2) & "00" when page /= ZERO else
	-- 				 '0' & instr_address_wop & "00";

	-- instr_address <= instr_address_full(30 downto 2);

	-- data_address_full <= "000" &
	-- 				data_address_wop(ADDR_WIDTH-1 downto PAGE_NUMBER_H_INDEX+1) & 
	-- 				page(PAGE_NUMBER_H_INDEX downto PAGE_SIZE_H_INDEX+1) & 
	-- 				data_address_wop(PAGE_SIZE_H_INDEX downto 2) & "00" when page /= ZERO else
	-- 				'0' & data_address_wop & "00";

	-- data_address <= data_address_full(30 downto 2);

	reset_cpu <= '1' when reset = '1' or reset_reg /= "11111" else '0';

	--synchronize reset --and interrupt pins
	intr_proc: process(clock, reset, reset_reg)
	begin
		if reset = '1' then
			reset_reg <= "00000";
		elsif rising_edge(clock) then
			if reset_reg /= "11111" then
				reset_reg <= reset_reg + 1;
			end if;
		end if;
	end process;

    clarvi: entity work.clarvi
		generic map(
			INSTR_ADDR_WIDTH 		=> ADDR_WIDTH,
			DATA_ADDR_WIDTH 		=> ADDR_WIDTH
		) port map(
			clock 					=> clock,
			reset 					=> reset_cpu,
			trace					=> trace,
			cpu_number				=> cpu_number,
			main_address 			=> data_address_wop,
			main_byte_enable 		=> data_byteenable,
			main_read_enable 		=> open,
			-- main_read_data_valid    => '1',
			main_read_data 			=> data_readdata,
			main_write_enable 		=> data_write,
			main_write_data 	 	=> data_writedata,
			main_wait 				=> data_waitrequest,
			--avm_main_readdatavalid 	=> data_readdatavalid,
			instr_address 			=> instr_address_wop,
			instr_read_enable 		=> instr_read,
			instr_read_data 		=> instr_readdata,
			instr_wait 				=> instr_waitrequest,
			--avm_instr_readdatavalid => instr_readdatavalid,
			inr_irq 				=> interrupt,
			interrupt_ret			=> interrupt_ret,
			ecall_signal			=> ecall,

			page_debug				=> page,

			cpu_mem_pause			=> cpu_mem_pause,

			--current_page			=> current_page,
			debug_register28 		=> open,
			debug_scratch 			=> open,
			debug_pc 				=> open
		);
	-- clarvi #(
    --     .DATA_ADDR_WIDTH(DATA_ADDR_WIDTH),
    --     .INSTR_ADDR_WIDTH(INSTR_ADDR_WIDTH),
    --     .INITIAL_PC(INITIAL_PC),
    --     .DEFAULT_TRAP_VECTOR(DEFAULT_TRAP_VECTOR)
    -- ) clarvi (
    --     .cpu_number         (cpu_number),
    --     .trace              (trace),
    --     .main_address       (avm_main_address),
    --     .main_byte_enable   (avm_main_byteenable),
    --     .main_read_enable   (avm_main_read),
    --     .main_read_data     (avm_main_readdata),
    --     .main_write_enable  (avm_main_write),
    --     .main_write_data    (avm_main_writedata),
    --     .main_wait          (avm_main_waitrequest),
    --     .instr_address      (avm_instr_address),
    --     .instr_read_enable  (avm_instr_read),
    --     .instr_read_data    (avm_instr_readdata),
    --     .instr_wait         (avm_instr_waitrequest),
    --     .clock              (clock),
    --     .reset              (reset),
    --     .inr_irq            (inr_irq),
    --     .interrupt_ret      (interrupt_ret),
        
    --     .ecall_signal       (ecall),
    --     .page_debug         (page),

    --     .cpu_mem_pause      (cpu_mem_pause),

    --     .debug_register28   (debug_register28),
    --     .debug_scratch      (debug_scratch),
    --     .debug_pc           (debug_pc)
    -- );
end architecture;