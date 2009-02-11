using System;
using System.Collections.Generic;
using System.Text;
using System.Net;
using System.Web;

namespace EMX
{
    public class SupportCloud
    {
        private string _cloudweb;
        private int _port;

        public SupportCloud(string cloudweb, int portNumber)
        {
            _cloudweb = cloudweb;
            _port = portNumber;
        }

        public void putData(string cloud, string key, Dictionary<string, string> data)
        {
            // Use the GET method of putting data on the cloud

            string parameter = convertData(data);
            string url = String.Format("http://{0}:{1}/cloud/put/{2}/{3}/?{4}", _cloudweb, _port, cloud, key, parameter);
            WebRequestHelper helper = new WebRequestHelper();
            string response = helper.GetResponse(url);
        }

        public void putLargeData(string cloud, string key, Dictionary<string, string> data)
        {
            // Use the GET method of putting data on the cloud

            // string realKey = key.Replace('/', '_');
            // string url = String.Format("http://{0}:{1}/cloud/put/{2}/{3}", _cloudweb, _port, cloud, realKey);
            string url = String.Format("http://{0}:{1}/emx/put", _cloudweb, _port);
            WebRequestHelper helper = new WebRequestHelper();
            try
            {
                string response = helper.PostResponse(url, data);
            }
            catch (Exception e)
            {

            }
        }

        public void putMessage(string mailbox, string clientid, string from, Dictionary<string, string> data)
        {
            string url = String.Format("http://{0}:{1}/mbox/put/{2}/{3}/{4}", _cloudweb, _port, mailbox, clientid, from);
            WebRequestHelper helper = new WebRequestHelper();
            string response = helper.PostResponse(url, data);
        }

        private string convertData(Dictionary<string, string> data)
        {
            StringBuilder b = new StringBuilder();
            foreach (string key in data.Keys)
            {
                // xml is "special"
                if (key == "xml")
                {
                    byte[] byteData = System.Text.ASCIIEncoding.ASCII.GetBytes(data[key]);
                    string encodedData = System.Convert.ToBase64String(byteData);
                    b.Append("xmlencode");
                    b.Append("=");
                    b.Append(encodedData);
                    b.Append("&");
                }
                else
                {
                    b.Append(key);
                    b.Append("=");
                    b.Append(HttpUtility.UrlEncode(data[key]));
                    b.Append("&");
                }
            }
            return b.ToString().Substring(0, b.Length - 1);
        }
    }
}
