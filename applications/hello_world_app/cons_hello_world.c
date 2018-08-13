#include <api.h>
#include <stdlib.h>
Message msg;
int main(){
    int i;
    Echo("task cons_hello_world started.");
    Echo(itoa(GetTick()));
    for(i=0;i<250;i++){
        Receive(&msg,prod_hello_world);
        Echo("Message received");
    }
    Echo(itoa(GetTick()));
    Echo("task cons_hello_world finished.");
    exit();
}
