# STAX: Single Threaded Asynchronous Execution Framework

STAX is a framework for developing single threaded asynchronous applications, similar to Javascript.
It does so by organizing code execution in different `Tasks`, which are scheduled non-preemptive on a single thread. This avoids the problems typically associated with threading, such as race conditions or deadlocks, while providing the ability to write asynchronous and concurrent code.

## Tasks and Scheduling
The main idea is to organize the code into small `Tasks`, which can be executed by a scheduler, the `Executor`.
The scheduler is non-preemptive, which means it can not by itself interrupt the currently running `Task`, but rather the `Task` itself must signal to the `Executor` that it can be interrupted.
This is for example the case when the `Task` has to wait, e.g. for another `Task` on which it depends to finish.

The goal hereby is to break down programs into `Tasks` so small, that no single `Task` will starve the others for processing time.
To do so `Tasks` can call other `Tasks`, to execute functionality but also to compute results which can be then used by the calling `Task`.
The calling `Task` then `awaits` the called `Task` and during this waiting, will hand over to the scheduler until the called `Task` finished, also allowing other, independent `Tasks` to be scheduled.

Besides `awaiting` another `Task`, a `Task` can also `yield` to the scheduler directly.
This can be used when waiting, e.g. when `sleeping` or when waiting for system resources.
During such waiting periods the `Task` can yield to the scheduler to allow other `Tasks` to be executed during that waiting period.

Through this mechanism `Tasks` will only be interrupted in fixed points, when the `Task` is not doing anything critical, and can therefore not result in race conditions between `Tasks`.
This completely eliminates the need for locks or critical sections as well as the problems associated with such locks, such as deadlocks.

## Generators
Besides `Tasks` there are also generators.
These are long running (potentially infinite) tasks, that produce multiple results during runtime.
A task can create a generator and use it to generate values.
When the task require the generator to generate a new value, it will schedule the generator and go to sleep until the next value was generated.
After generating a value, the generator will sleep and not be scheduled until the next value needs to be generated.
For this purpose the `AwaitNext<ResultType>` member of both `TTask` as well as `TGenerator<ResultType>` exists.

To create a generator, either inherit from `TGenerator<ResultType>` and override the `Execute` method, or use a function pointer of the signature `procedure GeneratorFunc(Yield: specialize TYieldFunction<ResultType>[; Args])` and create a generator from this using the `AsyncGenerator` functions provided in the `stax.functional` unit.

As generators might have potentially infinite execution time, unlike `Task`s they can not be simply terminated when finishing.
With the the memory of the `TGenerator<ResultType>` can only be freed after the execution finished, it does not use the normal manual memory management as `Task` which can simply be awaited and Freed afterwards.
Rather than that STAX makes use of reference counted `COM` interfaces.
So when using generators don't use the `TGenerator<ResultType>` type directly, but always reference them through the `IGenerator<ResultType>` interface.

Generators can also be iterated with `for VarName in Generator`.

Examples for using generators can be found in the `examples/generators` directory.

## Use Cases
The two main use-cases for such a system are first event based or interactive systems and second I/O.
### Interactive Systems
The first use-case is the implementation of interactive systems, which is the reason why Javascript uses a similar approach.
An interactive system must be responsive to incoming events.
For example in a GUI application, the application needs to be able to react to user inputs immediately, e.g. react to a button press.
Long uninterrupted processes can starve the event loop and make the interface non responsive.

In an asynchronous `Tasks` based system, such events can be scheduled as `Tasks`, which can be scheduled when the longer lasting processes yield time to the scheduler.

### I/O
I/O operations ofter require waiting for data from the communications partner.
This is usually implemented using blocking behavior.
For example a call to `ReadLn` will block the execution of code until the user has inputted something into the terminal.
So I/O heavy programs tend to spend a lot of their execution time waiting for data, while blocking other code execution.
This can be compensated by using `Threads`, and having the blocking operations only block a single thread, while the other threads can continue working.

But as discussed earlier, the usage of threads requires dealing with race conditions, e.g. via locking, which can result in deadlocks.
Asynchronous I/O solves this problem by simply yielding the `Task` while waiting, allowing other `Tasks` to be executed, without having any race conditions, as the code is still single threaded and can only be interrupted at safe points in time.

## Implementation
Tasks are implemented through the `TTask` class for tasks that don't produce any results and `TRVTask<T>` (Return Value Task), which produces a result of type `T`, which can for example be read out by other tasks after the task finished.
To implement a task, simply inherit from either `TTask` or `TRVTask<T>` and implement the `Execute` method (similar to `TThread`).
The result of `TRVTask<T>` has to be written into the field `FResult` which has `protected` visibility.

To ease implementation the unit `stax.functional` implements tasks that can be created from function pointers.
At the moment it supports creating tasks from any function or procedure (both as simple function or as method/of object) with 0 to 4 parameter.
Typing is handled via generics
```pascal
procedure Foo(AExecutor: TExecutor; AString: String);
function Bar(AExecutor: TExecutor; AInt: Integer): Double;
...
myProcTask := AsyncProcedure<String>(@Foo, 'Hello World!'); // create a task that will call Foo with argument 'Hello World!'
myFuncTask := AsyncFunction<Double, Integer>(@Bar, 42); // creates a task that will call Bar with argument 42
```

The scheduler is implemented in the `TExecutor` class.
New Tasks can be scheduled to the `TExecutor` class via the `RunAsync` method.
The scheduler will then work on the tasks in it's `Run` method, and will do so until no more `Tasks` are queued.
```pascal
Executor := TExecutor.Create;
Executor.RunAsync(AsyncProcedure(@Test)); // Initial task to run
Executor.Run; // Run until all tasks are finished
Executor.Free;
```

### Await and Sleep
Tasks can yield to the scheduler by calling their `Sleep` method.
There they can specify on how long the scheduler should wait at least before rescheduling them.
A `Sleep(0)` will yield to the scheduler but immediately reschedule the current task, so while giving other tasks a chance to be executed, if no other tasks are available will be continued directly.
To schedule another task and wait for it to finish, they can make use of the `Await` method.
Tasks can also be scheduled with `ScheduleForAwait` and be awaited afterwards.
To await multiple Tasks the `AwaitAll` method can be used.
It can be configured to either ignore exceptions, terminate all Tasks once the first exception was raised, or accumulate exceptions and raise them after all tasks finished or raised an exception.

The Await method also takes a `TimeOut` argument, which will terminate the task if it takes to long to complete. In that case an exception is raised to notify the user.

If all active tasks are awaiting or sleeping, the scheduler will call the the systems `Sleep` function to get load of the CPU.
In the case where there is always at least one active task, it will never go to sleep and easily reach 100% CPU load.

As there can only be one executor per thread, the executor of the current thread can be accessed via the global `GetExecutor` function.
Also a global `AsyncSleep`, `Await` and `AwaitAll` function is provided, which will call the respective method for the executor of the current thread.
`Await` can also be used to receive the result of an `TRVTask<T>`
```pascal
function MyFunc(AExecutor: TExecutor): Integer;
...
procedure MyTaskProcedure(AExecutor: TExecutor);
var
  i: Integer;
begin
  i := Await<Integer>(AsyncFunction<Integer>(@MyFunc)); // execute MyFunc as Task and fetch the result after it finished
end;
```

## Exceptions
When a `Task` raises an exception which is not handled within the task, it is stored within the `TTask` object and can be used later on.
STAX provides two standard ways to deal with such exceptions that can be configured when scheduling the task.
First there is the `OnError` event of the `TExecutor`, which can be called if a `TTask` finished with an unhandled exception.
The second way is to raise that exception to the `Task` that awaits the failing task:
```pascal
try
  Await(PossiblyFailingTask);
on E: Exception do
  // handle exception from the awaited task
end;
``` 

## Termination
A tasks can be terminated at any time by calling its `Terminate` method.
This method will then set the `Terminated` property of this task to `True`.
When the task is yielded while this flag is set an exception `ETaskTerminatedException` is raised to notify the task.
Unlike requiring the task to regularly check the `Terminated` flag and decide when to stop, this forces the task to act when it was terminated.
If a task awaits a terminated task, it will be woken up with an `EAwaitedTaskTerminatedException` being raised.

To stop the execution of the `TExecutor`, it also provides a `Terminate` method.
This will cause the executor to, once it gets back control from the current task, to terminate and wake up all remaining tasks.

A terminated task also has some restrictions.
It cannot yield to the scheduler, sleep or await other tasks.
Basically once terminated it must finish in the same scheduling cycle.
This ensures a timely termination after the call of `Terminate`.


## GUI Applications
STAX can be used within GUI applications. While there is no direct integration into the LCL it can be hacked together:
1. Add an Executor variable to your form
```pascal
TForm1 = class(TForm)
[...]
private
  FExecutor: TExecutor;
```
2. Add a global function like this:
```pascal
procedure ProcessMessages(AExecutor: TExecutor);
begin
  AExecutor.Sleep(10);
  AExecutor.RunAsync(AsyncProcedure(@ProcessMessages, 16 * DefaultTaskStackSize));
  Application.ProcessMessages;
end;
```
3. Add a timer (e.g. with the name StaxStartTimer) with an interval  thats large enough such that the whole form has loaded before fireing (100) with enabled to true (or, set it to a very small interval and enable it in the forms OnActivate event, but make sure to not activate it twice, e.g. by checking if FExecutor is Assigned). In that timers event create the executor and let it run with the ProcessMessages function:
```pascal
procedure TForm1.StaxStartTimerTimer(Sender: TObject);
begin
  StaxStartTimer.Enabled := False;
  if Assigned(FExecutor) then
    Exit;
  FExecutor := TExecutor.Create;
  try
    FExecutor.OnError := @Self.HandleTaskError;
    FExecutor.RunAsync(AsyncProcedure(@ProcessMessages, 16 * DefaultTaskStackSize));
    FExecutor.Run;
  finally
    FExecutor.Free;
  end;
end;
```
4. Add an `OnClose` event to the form that terminates the Executor:
```pascal
procedure TForm1.Form1Close(Sender: TObject);
begin
  if Assigned(FExecutor) then
    FExecutor.Terminate;
end;
```

### Explanation:
The `ProcessMessages` function will take over as the main message loop of  the LCL.
It will reschedule iteself before actually calling `Application.ProcessMessages`.
The reason for this is that now tasks can yield during event handlers, and while they are yielded, the event loop can start again and start serving the next event.
This way you can use `AsyncSleep` and `Await` within event handlers without having to worry about freezing your application.
The `Sleep` inside is to not have the application loop alone utilizing all of the CPU power which would result in having the application taking 100% CPU usage.
The LCL message loop can, depending on the complexity of event handlers and the widgetset easiely exceed the 4k stack size, so we set it to 16 times that (64k).
If that is not enough for your application, change this value accordingly, but be aware, larger values might result in performance loss which could result in lagging.

The timer is required to start the STAX event loop, any earlier events like `OnCreate` or `OnActivate` need to return, otherwise the application will freeze.
It is very important to set an error handler, otherwise a single uncaught exception will kill the whole STAX loop. Again use the same stack size here as in the ProcessMessages function.

The `OnClose` event is fired when the user requests closing of the form, but because we have still the `Executor` running the LCL can't finish the application.
So to be able to close the form and stop the application, we need to tell the `Executor` to stop by calling it's `Terminate` function.

An example for this can be seen in the `pong` example, which integrates STAX into the LCL to create a multiplayer game of pong, where LCL controls are used for displaying the game and catching user input, while communicating with the other player via the stax `asynctcp` unit.

## Examples
The examples directory contains a few small examples.
* `tasktest` provides a simple program that will call two tasks that count to 10, and will be scheduled parallel to each other as they yield each iteration when awaiting the printing task that will output the current number.
* `exceptionstest` provides a small example of catching exceptions from another task
* `tcpexample` implements a simple tcp echo server and tcp client, which runs completely single threaded and can handle multiple connections simultaniously
* `stoptest` shows how to stop running tasks
* `awaitalltest` shows the usage of the `AwaitAll` function  with respect to exceptions
* `timeout` contains an example for using timeouts with `Await`
* `pong` provides a simple two player pong game via TCP, which incorporates STAX into LCL GUI applications
* `generaotrs` this directory contains 3 examples on how to use generators, a simple generator generating 3 numbers, a generator iterating through directories recursively and an infinite generator.
