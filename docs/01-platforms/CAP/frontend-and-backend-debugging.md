# Debug in CAP Java

## Prerequisites

1. Spring Boot Dashboard&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=M2RkNDdkYTcxOTkyZWFkYmRhZTljZjZhMzk0MzhlMjZfT3pLMkdjN0Fvc0ZmdEw2WjdxRjJPcVhQdFN5dTZOUzlfVG9rZW46U3dTT2JvQ2FFbzg0UjB4TkxLUmNwVHJqbk5mXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

After you download the extension, it will appear in the sidebar.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MGVlNjdkZjI5NDZjMTU3ZDllMTAzZDE2MjQ3Nzk4NjBfc1piYmlTa2o5M2xFSUpZMWF2Q1hwbE81NHRPckxIY2VfVG9rZW46UzUwNGJScmhmbzFDcFJ4ekdYbWNsMHRBbnNjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Debugger for Java

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MTQwMmUyODI3YjNiYzgyNTM0MWRjZTYwMmMzMzYxYzBfaHVYVjdtdFVWQWJDdHc3UkJRNFJXNHA2YjdsNm9rc1pfVG9rZW46VFZocmJobUxyb1hIcmF4ZFhIUmN3OVowbkZlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

# Debugging Panel

## Variables

For the variable panel, it shows current variable values when the application is paused at a breakpoint.

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=Yjc1ZmQ0YjMzNDc1NWIyZGRmZTBjM2RiZWJjZGIxODBfRDQyd3IyNkhrakZoaDJNUVY0akRWQU9rVGdMVEh3UHBfVG9rZW46SU9lQWJvU3FSbzBwUFJ4QktLMWNST05jblVkXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

## Watch

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NTM0NzQ1OWRjMGI3MDRjZWQzMzIzZTBmNjEzN2JlMDBfYXM0VktmVkNRY0J4akMyeVFXQ0NRVjM1U1J1OFhNVTdfVG9rZW46V3NzMmI3cUpZb2s0Y2J4Mm5sUmM4cDVobmNlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

For this feature, you can monitor specific expressions, variables, or calculations in real time during debugging. It will only show you what you want to monitor through expression. In this case, I'm using botInstance.getId().equals("48939434-122b-48d4-98ed-b0565d16329c") to specify the bot instance i want to monitor. But the bot instance that exists is (8939434-122b-48d4-98ed-b0565d16329c). So it will not trigger the breakpoint because the expression is not the one i want to focus on.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NjhlYTY3ZDZiNWEyODY2NzNjYjY0ODM3NmRkODdiNWVfY0hZM0hsUVVhZFRQVFVxc25XeWVrRDVtNm9EYnZvWGJfVG9rZW46T0Y1S2JpYzY0b285UXZ4ZjdSNGN6OVRWbktGXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

## Call Stack

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=M2E3ZjU3ZTgzMmFhMWNiODZiMzI4NTVmNWNiMTI1ODRfcVVRTzR1OEtLalljOHF2aVRrQ2xzMEFHWllMdWs0SzRfVG9rZW46U05XMmJkYVc1b1k4V3d4VFJIMWNHd3czbnhmXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

The call stack is a live view of the program's execution at the moment it paused (at breakpoint, exception, or manual pause). Each line represents a function call that's currently "active" - meaning it was called but hasn't returned yet. You can click different stack frames to jump between function context. You can also double click a frame to jump to that code location.&#x20;

For practical debugging uses, you can tracing execution flow and finding the root causes if theres an error by look down the ctack to see what sequence of calls led to it. Besides that, you can identify infinite loops by see the same function repeated many time in the stack.&#x20;

## Breakpoints

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=OGI0NjRkYzMwZmQyNzNlNTRjNGYxNTc2Yzc3NzRhYjhfbjYwdzZlZUlCWkdMSEhmSEE5dUx0VjY2VlJWQnk0aElfVG9rZW46Q1cwUmJ3S0htb1B4VFR4cFd5bGNOOWpwbnNmXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

Breakpoint is a tool that we set in the source code to intentionally pause the application when it reaches that point during execution. While paused, we can inspect values, step through the code line by line, and understand the control flow and data.&#x20;

## Loaded Script

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MTIwMzJmZjVlNTk4Y2JmNDcxNzRjNzMyYTI3ZjYyMTNfNTNmTDkwUHBEUFBua2VqUGNBRG40RzFsTWUwYWxLUnVfVG9rZW46WHlqYWJjaDZub1hqZUF4ZmhlRGNGZ3hSbkJiXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

Loaded Scripts panel in VS Code shows JavaScript files that are currently loaded and available for debugging. This panel means that you're debugging SAP UI frontend served by your CAP Java backend. You can expand the app folder and search for your frontend code and find JavaScript.

# Backend (Java Springboot)

1. Step 1

Choose what we want to debug first. For example, in this case we want to debug chat completion related to bot service chat&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MjRmYjJhMDIzMGYxYTA3MGQ0NDRlN2EwZTQzOGU1MmJfRVQ3d25UeGdmY2hHY0Nlamx1U0Q2UUZkSk1PUHRRWXNfVG9rZW46VFBUdGJjZTlVbzR1TG14UlcxSmNIeWt0blZlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Breakpoint

Put breakpoint in the related&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MDdiMjU1NGQ2NTk3MzYyYjUxZTZkYzIxNmEwMmJiNzlfS2FNOVE4YVBmcXVqVkRyNEpKYnAxdk1EeHdoRk9VRTdfVG9rZW46R2lkQmJBVmNZb1hadkZ4b3ZQeGNWcXdTbkRkXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Run and Debug

- Go to spring boot run in the side bar, and click debug

- It will run the program and debug it automatically. We can see that at the bottom of the VS code turned purple (sometimes it turned red)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MzZjNDQ1YmVlYTNlYjI0NmQ2OGNiZDVlZjkxYjI1N2RfZDF4YWE5SlR5SnI5cGFtZHVFdlVPMTJISjJlbVF2RDJfVG9rZW46SnNoN2JxVXhYbzlSRzZ4azJyYmNQZmg5bnBlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NzVkZDgxMjA3MmY2YzIzZDUzODJlNTU0MDJhNzc4ZjJfNDBzVVZpb2JJYVdyWkVSVDN1Y2ZIZlc1MGpRSlp6eGdfVG9rZW46UnFVSGJwZDBQb0hNQkV4QXROY2NLQ0d3bmFmXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Test it through Postman

- Since in this case we want to debug chat completion, we should put chat completion URL POST.&#x20;

- You can see there's notification in VS code that the debug is running&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=N2EzNmU0MjQ5M2FlN2RiYzkwYTdjMDM5YTZhMDg5MGRfZTJ4NVFSTmpYVXBBWEZReW1tNVlRRUdMRkk2RmRucFhfVG9rZW46TDBveWJIb1pabzZUY3R4VkJWbWNJdEZDbkFlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZjIxNzFlZjFkODVjNzM3MDk1NWVkMjQyZmQ5NjcwYjNfYU9tQXBNNWg3dWdDU2pJdFhKWlJGaEh0NXVpT05BSlZfVG9rZW46RDRNT2JSN1dib0ZUWUZ4Wmh1UGNSc0JxbmhjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Result&#x20;

The result is come out without an error

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NDkxNzNiZjVhNmM3YmVmNTM5YjlmM2UwYjkzOGQyNjRfcVRtT2hWRDdkck9KWVdCejJtU3ZLVHJnY1k0TnVlRjZfVG9rZW46SVd2Y2JFSVZ1b2NkSWN4OVFHVGNHZldFbkNlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)





# Frontend (Javascript)

## Frontend (Javascript)

## Modify launch.json

To debug JavaScript frontend, we need to add this configuration:

```bash
                {
                        "type": "chrome", //can change it to your browser
                        "request": "launch",
                        "name": "Debug JavaScript Frontend (served by backend)",
                        "url": "http://localhost:8080",
                        "webRoot": "${workspaceFolder}/app"
                }
```

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MDZlOGZhNzA0MzM0MDlhNjlmYmE2Mzg1YTMxZDk4MThfdmdJVG0zTFdxV0J2cGZjOXFOeGdVcHBmZE5ZMzVHeGFfVG9rZW46UEtPaGJYMmlXb1psU0d4UW1aTWNTbFg3bjhiXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

1. Set breakpoints

Since we need to debug the frontend, we need to set the breakpoint in frontend. In this case, we set the breakpoint in the \_handleAdoptClick function.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZWJlOTkzZjQ0ZmRiMTZiNGYyOGQyOTc1MWEyOWZkNTFfb29zYjhSWHhPU3hrM1IweXFxTzEyTWoyZ1dZNmJuWkRfVG9rZW46WWtUZ2JKNVJjb1lwUFp4d3FERGNLYk01bks3XzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Run the debug

Go to Spring Boot Dashboard and run the debug. This dashboard has a function to run debug in the application.&#x20;



![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NzA4YWUwN2Q0MjQ0NjVkZjk5Nzg4ODgwY2QxYjY2OTZfeDZmSDJBSzZ0YXJmM0V3TXAzZXAwQXpKUmFLQjlkQWFfVG9rZW46TnFvSWIwM1pUb3NRbjN4dkJnUGN0aGVVblFiXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Open the chrome debug

Because VS Code can't detect chrome that doesn't have debug, we need to open chrome from debug. We can see that chrome has debug in it from call stack.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MTZmMDMxODU5NzhlZjcyM2RhY2IzMTk0MGE1NzJhNzVfMkp3ZXdwY2R4bGVuM2Z4d2RPbXd2d2d6UFV1RWQ5TTlfVG9rZW46THVZUmIzSHpPb3VMWE54WFk0TmNPRktOblJnXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)





* Chrome Opens

After chrome opens, go to /task-runtime



![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=OTIxZjYzOGM3YTBkZmFkN2UzODMyMWVkYzMzMzUzYThfZ0F0TFRRM1RTODloN3d2VjM3anZaR2x5SVl4ZGZrWTlfVG9rZW46RFNmY2I2TkdpbzN2MUF4dno1N2M4ek9ObjdmXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Go to the data

Click the data&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ZGY4ODgyMjcwZDRmNGEwMjdiMmQ1MzI4N2I3OGJlOTJfYlkzNzY5YmdWMmpRM2Q0Wkx1VzJIb2sxNzlnQkduRHFfVG9rZW46S0x4SWJWNDVsbzYwUGJ4b2FYamNpYTlQbkFkXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* &#x20;Test breakpoint we set

In this case, we debug function in frontend specifically in adopt handler button function. That means we need to test the adopt button.&#x20;



![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=YmU2YjQ4ZWVkYmU3ODVkMTkxNDM3MzQwMDNkZjY1OGZfREg1NkNoOHp6TlpMbk4wbXR1TmNjbW1tcTdibTRQM3lfVG9rZW46V3pHdmJoZzRTb2pLazd4cHUxQWM3d3FEbk9ZXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

* See the debug result

After that, we can go to VS Code again and see whether the debug is working or not.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NzQ5MTU2YmM0YTE0MTgwMDA3NTlmNDRjYzE5Y2EwMzhfNVMxcENOVWpuQjg3ZHNRWjdjWEJyR3dGcnNNSjZwU3JfVG9rZW46UVk5SmJQdW5Pb1VpR3d4bWhVNmM1aVBSbnJoXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



## Loaded Script

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MjQ5OWU0YzNjYjY5NjNmNDQxNDczNTcxNDExMWFjOWRfcWRBR2JoT005UnhLYWdENnhOcWx0bmJyVTBRV2Vod0FfVG9rZW46S3Z4Y2JqYmFTb1pOUEh4UmQ2OGNNN3NCbkdlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

Loaded Scripts panel in VS Code shows JavaScript files that are currently loaded and available for debugging. This panel means that you're debugging SAP UI frontend served by your CAP Java backend. You can expand the app folder and search for your frontend code and find JavaScript.

# Appendix

## Function in Debug

1. Continue

This function is for resuming until the next breakpoint and skipping to the next checkpoint.

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=ODE0YmM4ZDEyOWI0YWJlYjM3ZTQxZjY1Njg4Mzg3YzVfblVFTmJOYk5GUXAwQjl1aUk4aEVSZkw5N29UYUhVRUxfVG9rZW46VHFJVGJqSXFKb2NjeEp4NWJOMGNOb0lBbnYzXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Step Over

Step over (F10): this function is to execute the current line without entering methods and stay at current level

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MmE3M2FhYTNjOWQ3OTljN2YxYWJiOTUxYTkzNzYzMGVfdUgxcGV2THN5ZDdGNldwSDE1MXhObEdDVHRWbzdVZkxfVG9rZW46VENiS2JFem1Wb09Gc3R4a0dSZGNLdWhvbnNlXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Step Into

- Step Into (F11)" this function is to enter into method call and deep dive into CAP framework

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=OTUwZjhhNjU0OTQwZjE1MDYzMjUzYThhZTAyNTVkODRfVllNU1B5YjV1RnZvOWRJQlhIOVpBbGdWN3JkSmpqYWtfVG9rZW46R0Q0Q2JXRjZmb2Rwcjd4VTJrWmNyT2FUbkZjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)





* Step Out

- Step Out (Shift+F11): this function is to exit current method and return to caller.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MDM0MGM4YWJlZTA0MWZiNWFjMzM2MTQ1ODExOWY5ZDNfYnB4bkkwaTBJZTc0emdRSG1FV2Vzb2VVYkJFdUNTQlhfVG9rZW46WUsxR2I1Wlo2b0IwZGx4TDMwUGNicXJTbkdkXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Restart

- Restart (Ctrl+Shift+F5): this function is to stops the currently running debug session and immidietly launches a new debugging session with the same configuration.



![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=Mjk2YTgyMzY4NWJkYTFkMjlhNmZlMDdiMjM5MTYyNTJfb1RjRDBpaHZ2akYxd090S0VsdWNJbU5vNW42WWhiUmpfVG9rZW46Q2ZvNWJod1hMb1N0ZUx4aU01UGNybUZXbnRmXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Stop&#x20;

- Stop (Shift+F5): This function terminates the current debugging session completely. The current debugging session and shuts down the running application without starting the new one.&#x20;

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NjgxNzJhMmM0MzdhNzlkZDI0N2EwOTVkNmUxMDE2OGRfMDl2S1Y3QmNHNEh3ZUYxeGtPV2NncTlTZWd3SkxXM2xfVG9rZW46QWpLQWIyaDBCb3ZsNEh4bHZQSmMyNlgwbjFjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* Hot Code Replace

- This feature allows you to modify Java code while the application is running and apply those changes without restarting the entire application

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=NTdhOWE1ZmEwOTdiYTE4YzgzNzkyNzkwNGI2MDYxMTVfZEJmenhveTRXS0k5SElYeVAwNHRzVTlXakdZOXU3a1hfVG9rZW46R09wRGI2aXVRb0VIb3h4Y3hNWWNsVFZYbmxjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)



* For this feature, there are code that can be replaces such as simple logic, variable values, method implementation, and method bodies. But for new methods, new fields, and changing method signatures, you need to restart the debug. Exapmle:&#x20;

```typescript
@Component
@ServiceName("CatalogService")
public class CatalogServiceHandler implements EventHandler {
    
    @On(event = CqnService.EVENT_READ, entity = "Books")
    public void onReadBooks(CqnSelect query, EventContext context) {
        // ✅ You can change this logic without restart
        System.out.println("Reading books - NEW MESSAGE");
        
        // ✅ You can modify variable values
        String greeting = "Hello World - UPDATED";
        
        // ✅ You can change method implementation
        if (query != null) {
            // New conditional logic
            processQuery(query);
        }
    }
    
    // ✅ You can modify existing method bodies
    private void processQuery(CqnSelect query) {
        // Updated implementation
        System.out.println("Processing query differently");
    }
}
```

* Step by step to use Hot Code Replace: start debugging → set breakpoints → trigger the code → pause at breakpoint → edit the code → save the file → continue debugging (F5) → changes applied

> ### **Visual Indicators:**
>
> * 🔥 **Hot swap successful**: No restart needed
>
> * ⚠️ **Hot swap failed**: VS Code shows notification "Hot code replace failed"

## Variable not came out in debugger

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=Y2M1MmM2ZTljNjg3MTk1N2U4ZGYzNWY1OTY3ODFjNTJfRm9maHVWY2hXWE5YbVJiSkNyOXV5VVllR2tDaGpVM0NfVG9rZW46UFdxQmJNVFd6b1liWEN4OVhqdmNlb3RmbkxiXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

![](https://u0vocx8xrmg.feishu.cn/space/api/box/stream/download/asynccode/?code=MjE5OGU0ZDcxNTI3MWM5NGUzMzhjMmFjZDRkMzQ1M2Jfd2hzVGhEdVVONlpBUFFvaXU5R2tlTnlBQ3JzTGIxNkRfVG9rZW46Q2JXbGJLWGd1b05KbG14MVFZTmNhcnVSblVjXzE3Njc5MjQ0ODk6MTc2NzkyODA4OV9WNA)

This happens usually because of watch or expression that we set is unable to catch the specify expression.&#x20;

So the variable result came out in postman instead in VS Code debug.&#x20;

##
