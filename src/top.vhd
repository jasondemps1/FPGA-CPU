library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
end top;

architecture top of top is
  component imem is
    port(im_addr  : in  unsigned(10 downto 1);
         im_instr : out unsigned(15 downto 0);
         clock    : in  std_logic);
  end component;
  component dmem is
    port(dm_addr    : in  unsigned(10 downto 2);
         dm_data_wr : in  unsigned(31 downto 0);
         dm_data_rd : out unsigned(31 downto 0);
         dm_we      : in  std_logic;
         clock      : in  std_logic);
  end component;
  component rfile is
    port(radr1, radr2, radr3, wadr : in  unsigned(3 downto 0);
         wdata                     : in  unsigned(31 downto 0);
         rdata1, rdata2, rdata3    : out unsigned(31 downto 0);
         write                     : in  std_logic;
         clock                     : in  std_logic);
  end component;
  component Pipeline is
    port(
      clock                     : in     std_logic;
      im_instr                  : in     unsigned(15 downto 0);
      im_addr                   : buffer unsigned(31 downto 1);
      dm_data_rd                : in     unsigned(31 downto 0);
      dm_addr                   : out    unsigned(31 downto 2);
      dm_data_wr                : out    unsigned(31 downto 0);
      dm_we                     : out    std_logic;
      radr1, radr2, radr3, wadr : buffer unsigned(3 downto 0);
      wdata                     : buffer unsigned(31 downto 0);
      rdata1, rdata2, rdata3    : in     unsigned(31 downto 0);
      rf_wr                     : out    std_logic
      );
  end component;

  signal im_addr                       : unsigned(31 downto 1);
  signal im_instr                      : unsigned(15 downto 0);
  signal dm_addr                       : unsigned(31 downto 2);
  signal dm_data_wr                    : unsigned(31 downto 0);
  signal dm_data_rd                    : unsigned(31 downto 0);
  signal dm_we                         : std_logic;
  signal radr1, radr2, radr3, wadr     : unsigned(3 downto 0);
  signal wdata, rdata1, rdata2, rdata3 : unsigned(31 downto 0);
  signal rf_wr                         : std_logic;
  signal clock                         : std_logic := '1';

  signal reset : std_logic := '1';

begin

  --cpu_core  : core port map(im_addr, im_instr, dm_addr, dm_data_wr, dm_data_rd, dm_we, clock);
  cpu_pipeline : Pipeline port map(clock, im_instr, im_addr, dm_data_rd, dm_addr, dm_data_wr, dm_we, radr1, radr2, radr3, wadr, wdata, rdata1, rdata2, rdata3, rf_wr);

  data_mem  : dmem port map(dm_addr(10 downto 2), dm_data_wr, dm_data_rd, dm_we, clock);
  prog_mem  : imem port map(im_addr(10 downto 1), im_instr, clock);
  rfile_mem : rfile port map(radr1, radr2, radr3, wadr, wdata, rdata1, rdata2, rdata3, rf_wr, clock);

  process
  begin
    reset <= '0';

    wait for 20 ns;

    loop
      wait for 10 ns;
      clock <= '0';
      wait for 10 ns;
      clock <= '1';
    end loop;
  end process;
end top;
