using System;
using System.IO;
using System.Net;
using System.Text;
using System.Collections.Generic;


namespace EMX
{
    public class WebRequestHelperException : Exception
    {
        public WebRequestHelperException() : base() {}
        public WebRequestHelperException(string message) : base(message) {}
        public WebRequestHelperException(string message, Exception innerExcept) 
            : base(message, innerExcept) {}
    }

    /// <summary>
    /// Helper class to perform HTTP requests & return responses
    /// </summary>
    public class WebRequestHelper
    {
        private HttpWebRequest _webRequest;
        private HttpWebResponse _webResponse;
        private NetworkCredential _networkCredential;
        private CookieContainer _cookies;
        private string _uri;

        public WebRequestHelper()
        {
        }

        /// <param name="username">Username</param>
        /// <param name="password">Password</param>
        public WebRequestHelper(string username, string password)
        {
            _networkCredential = new NetworkCredential(username, password);
            string test = _networkCredential.ToString();
        }

        public WebRequestHelper(string username, string password, CookieContainer ck)
        {
            _networkCredential = new NetworkCredential(username, password);
            string test = _networkCredential.ToString();
            _cookies = ck;
        }

        /// <summary>
        /// HttpWebRequest created by the Helper
        /// </summary>
        public HttpWebRequest Request
        {
            get { return _webRequest; }
        }

        /// <summary>
        /// HttpWebResponse (only instantiated after GetResponse)
        /// </summary>
        public HttpWebResponse Response
        {
            get { return _webResponse; }
        }

        /// <summary>
        /// Prepare a Web Request adding username and password to credentials 
        /// if supplied in constructor
        /// </summary>
        /// <param name="uri">Web address, e.g. http://site </param>
        public void PrepareRequest(string uri)
        {
            _uri = uri;
            _webRequest = (HttpWebRequest)HttpWebRequest.Create(uri);
            if (_cookies != null)
            {
                _webRequest.CookieContainer = _cookies;
            }
            _webRequest.ReadWriteTimeout = 1200000;
            if (_networkCredential != null)
            {
                _webRequest.Credentials = _networkCredential;
            }
        }

        /// <summary>
        /// Prepare and Perform blocking HTTP request and get response
        /// </summary>
        /// <param name="uri">URL for request</param>
        /// <returns>String containing response to HTTP request</returns>
        /// <remarks>Performs an HTTP GET. Can be expanded to do POST etc.</remarks>
        public string GetResponse(string uri)
        {
            PrepareRequest(uri);
            return GetResponse();
        }

        public string PostResponse(string uri, Dictionary<string, string> parameters)
        {
            ASCIIEncoding encoding = new ASCIIEncoding();
            StringBuilder b = new StringBuilder();
            foreach (string key in parameters.Keys)
            {
                if (b.Length != 0)
                {
                    b.Append("&");
                }
                if (key == "xml")
                {
                    byte[] byteData = System.Text.ASCIIEncoding.ASCII.GetBytes(parameters[key].Replace('&', ' '));
                    string encodedData = System.Convert.ToBase64String(byteData);
                    b.Append("xmlencode");
                    b.Append("=");
                    b.Append(encodedData);
                    //b.Append("&");
                }
                else
                {
                    b.Append(key);
                    b.Append("=");
                    b.Append(parameters[key].Replace('&', ' '));
                }
            }

            byte[] data = encoding.GetBytes(b.ToString());

            PrepareRequest(uri);
            _webRequest.Method = "POST";
            _webRequest.ContentType = "application/x-www-form-urlencoded";
            _webRequest.ContentLength = data.Length;
            Stream newStream = _webRequest.GetRequestStream();
            // Send the data.
            newStream.Write(data, 0, data.Length);
            newStream.Close();

            return GetResponse();
        }

        /// <summary>
        /// Perform blocking HTTP request following PrepareRequest(uri) and get response
        /// </summary>
        /// <returns>String containing response to HTTP request</returns>
        /// <remarks>Performs an HTTP GET. Can be expanded to do POST etc.</remarks>
        public string GetResponse()
        {
            try
            {
                _webResponse = (HttpWebResponse)_webRequest.GetResponse();

                // Determine encoding from server, defaulting to CodePage 1252 if not possible
                Encoding enc;
                try
                {
                    enc = Encoding.GetEncoding(_webResponse.ContentEncoding);
                }
                catch
                {
                    enc = Encoding.GetEncoding(1252); // extended ASCII (Western European)
                }

                string responseString;
                using (StreamReader resStream = new StreamReader(_webResponse.GetResponseStream(), enc))
                {
                    responseString = resStream.ReadToEnd();
                }

                if (_webResponse != null)
                {
                    _webResponse.Close();
                }

                return responseString;

            }
            catch (Exception e)
            {
                string errMsg = "Failure requesting " + _uri;
                throw new WebRequestHelperException(errMsg, e);
            }

            //byte[] buf = new byte[8092];    // 8k buffer to read web responses
            //Stream resStream = response.GetResponseStream();
            //StringBuilder sb = new StringBuilder();
            //int count = 0;
            //do
            //{
            //    // Fill the buffer with data
            //    count = resStream.Read(buf, 0, buf.Length);

            //    // Append data read to response string
            //    if (count != 0)
            //    {
            //        string tempString = enc.GetString(buf, 0, count); // translate bytes to string
            //        sb.Append(tempString);
            //    }
            //}
            //while (count > 0); // any more data to read?

            //return sb.ToString();
            
        }

    }
}
