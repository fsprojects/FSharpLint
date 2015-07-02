/// FSharpLint, a linter for F#.
/// Copyright (C) 2014 Matthew Mcveigh
/// 
/// This program is free software: you can redistribute it and/or modify
/// it under the terms of the GNU General Public License as published by
/// the Free Software Foundation, either version 3 of the License, or
/// (at your option) any later version.
/// 
/// This program is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU General Public License for more details.
/// 
/// You should have received a copy of the GNU General Public License
/// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace FSharpLint.CrossDomain
{
    using System;
    using System.Collections.Concurrent;
    using System.Collections.Generic;
    using System.Linq;
    using System.Runtime.Remoting.Messaging;
    using System.ServiceModel;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;
    using FSharpLint.Worker;

    /// <summary>
    /// Worker written in C# to work around what seems to be a bug in mono.
    /// Use this worker to run lint in a separate application domain with isolated dlls.
    /// The bug is that running code in another app domain will load the original
    /// app domain's FSharp.Core if it's different. We're using app domains to isolate 
    /// the dlls used to run the linter so this bug is a problem.
    /// This worker gets around this by sitting between the two F# app domains, the
    /// first F# app domain loads this C# worker in another domain, this worker then loads
    /// the F# worker in yet another app domain - the FSharp.Core from the first domain
    /// is now no longer loaded into the F# worker app domain.
    /// </summary>
    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        /// <inheritdoc />
        public event ErrorReceivedEventHandler ErrorReceived;

        /// <inheritdoc />
        public event ReportProgressEventHandler ReportProgress;

        /// <inheritdoc />
        public FSharpLint.Worker.Result RunLint(string projectFile)
        {
            var appDomain = this.GetAppDomain();

            try
            {
                var worker = this.GetWorker(appDomain);

                worker.ErrorReceived += new ErrorReceivedEventHandler(this.HandleError);

                worker.ReportProgress += new ReportProgressEventHandler(this.HandleProgress);

                return worker.RunLint(projectFile);
            }
            finally
            {
                AppDomain.Unload(appDomain);
            }
        }

        /// <summary>
        /// Propagates lint errors to our own event.
        /// </summary>
        /// <remarks>
        /// This is a one way event so that the event is not run in the FSharpLint.Application domain.
        /// </remarks>
        /// <param name="error">Lint error.</param>
        [OneWay]
        public void HandleError(Error error)
        {
            this.ErrorReceived(error);
        }

        /// <summary>
        /// Propagates linter progress to our own event.
        /// </summary>
        /// <remarks>
        /// This is a one way event so that the event is not run in the FSharpLint.Application domain.
        /// </remarks>
        /// <param name="progress">Linter progress.</param>
        [OneWay]
        public void HandleProgress(Progress progress)
        {
            this.ReportProgress(progress);
        }
        
        /// <summary>
        /// Loads the FSharpLint worker from FSharpLint.Application in a 
        /// separate app domain and returns a proxy to the worker.
        /// </summary>
        /// <returns>Proxy to the worker from FSharpLint.Application.</returns>
        private IFSharpLintWorker GetWorker(AppDomain appDomain)
        {
            return (FSharpLint.Worker.IFSharpLintWorker)appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.FSharpLintWorker+FSharpLintWorker");
        }

        private AppDomain GetAppDomain()
        {
            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup
            {
                LoaderOptimization = LoaderOptimization.SingleDomain,
                PrivateBinPath = directory,
                ApplicationBase = directory,
                DisallowBindingRedirects = true
            };

            return AppDomain.CreateDomain("Lint Domain", null, setup);
        }
    }
}
