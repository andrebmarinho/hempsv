module memory_debug #(
    parameter ADDR_WIDTH = 29
)(
    input logic clock,
    input logic [ADDR_WIDTH-1:0] address,
    input logic read_enable,
    input logic [31:0] read_data,
    input logic write_enable,
    input logic [31:0] write_data
);

    always_ff @(posedge clock) begin
        // detect writes to certain addresses
        if (write_enable) begin
            casez (address[ADDR_WIDTH-1:0] << 2) // turn our word address into a byte address
                32'h04000000: $display("LED write, address %h, data %h", address<<2, write_data);
                32'h04000080: $display("HEX write, address %h, data %h", address<<2, write_data);
            endcase
        end
    end
endmodule