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

    public class FSharpLintWorker : MarshalByRefObject, FSharpLint.Worker.IFSharpLintWorker
    {
        /// <inheritdoc />
        public event ErrorReceivedEventHandler ErrorReceived;

        /// <inheritdoc />
        public event ReportProgressEventHandler ReportProgress;

        /// <inheritdoc />
        public FSharpLint.Worker.Result RunLint(string projectFile)
        {
            var worker = this.GetWorker();

            worker.ErrorReceived += new ErrorReceivedEventHandler(this.HandleError);

            worker.ReportProgress += new ReportProgressEventHandler(this.HandleProgress);

            return worker.RunLint(projectFile);
        }

        [OneWay]
        public void HandleError(Error error)
        {
            this.ErrorReceived(error);
        }

        [OneWay]
        public void HandleProgress(Progress progress)
        {
            this.ReportProgress(progress);
        }
        
        private IFSharpLintWorker GetWorker()
        {
            var fullPath = System.Reflection.Assembly.GetExecutingAssembly().Location;

            var directory = System.IO.Path.GetDirectoryName(fullPath);

            var setup = new AppDomainSetup { LoaderOptimization = LoaderOptimization.SingleDomain, PrivateBinPath = directory, ApplicationBase = directory, DisallowBindingRedirects = true };

            var evidence = AppDomain.CurrentDomain.Evidence;

            var appDomain = AppDomain.CreateDomain("Lint Domain", evidence, setup);

            return appDomain.CreateInstanceAndUnwrap("FSharpLint.Application", "FSharpLint.Application.FSharpLintWorker+FSharpLintWorker") as FSharpLint.Worker.IFSharpLintWorker;
        }
    }
}
