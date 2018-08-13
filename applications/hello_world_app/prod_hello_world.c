#include <api.h>
#include <stdlib.h>
Message msg;
int main(){
    int i, j,t;
    Echo("task prod_hello_world started.");
    Echo(itoa(GetTick()));
    for(i=0;i<250;i++){
        msg.length = 30;
        for(j=0;j<30;j++) msg.msg[j]=i;
        Send(&msg,cons_hello_world);
        Echo("Message sent");
    }
    Echo(itoa(GetTick()));
    Echo("task prod_hello_world finished.");
    exit();
}
