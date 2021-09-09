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
```
procedure Foo(AExecutor: TExecutor; AString: String);
function Bar(AExecutor: TExecutor; AInt: Integer): Double;
...
myProcTask := AsyncProcedure<String>(@Foo, 'Hello World!'); // create a task that will call Foo with argument 'Hello World!'
myFuncTask := AsyncFunction<Double, Integer>(@Bar, 42); // creates a task that will call Bar with argument 42
```

The scheduler is implemented in the `TExecutor` class.
New Tasks can be scheduled to the `TExecutor` class via the `RunAsync` method.
The scheduler will then work on the tasks in it's `Run` method, and will do so until no more `Tasks` are queued.
```
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
```
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
```
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

## Examples
The examples directory contains a few small examples.
* `tasktest` provides a simple program that will call two tasks that count to 10, and will be scheduled parallel to each other as they yield each iteration when awaiting the printing task that will output the current number.
* `exceptionstest` provides a small example of catching exceptions from another task
* `tcpexample` implements a simple tcp echo server and tcp client, which runs completely single threaded and can handle multiple connections simultaniously
* `stoptest` shows how to stop running tasks
* `awaitalltest` shows the usage of the `AwaitAll` function  with respect to exceptions
* `timeout` contains an example for using timeouts with `Await`
* `pong` provides a simple two player pong game via TCP, which incorporates STAX into LCL GUI applications