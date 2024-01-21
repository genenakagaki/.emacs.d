(ns emacs.preview.views
  (:require
   [cljs.pprint :refer [pprint]]
   [clojure.walk :as w]
   [emacs.preview.data :refer [org-data]]
   [emacs.preview.subs :as subs]
   [re-frame.core :as re-frame]))

(defn flatten-one-level [coll]
  (->> (mapcat #(if (sequential? %) % [%]) coll)
       (into [])))

(defn html-element [tag & contents]
  (->> (apply concat [tag] contents)
       (into [])))

(defn org-element->html [element]
  (let [contents (:contents element)
        props (:props element)]
    (case (:type element)
      "org-data" (flatten-one-level contents) 

      "property-drawer" nil
      "node-property" nil
      "keyword" nil

      "section" (flatten-one-level [:div.section contents])

      "headline"
      [:div 
       (flatten-one-level
        [(keyword (str "h" (:level props)))
         (:title props)])
       (flatten-one-level contents)
       ]

      "plain-list" (if (= (:type props) "ordered")
                     (flatten-one-level [:ol contents])
                     (flatten-one-level [:ul contents]))
      "item" (flatten-one-level [:li contents])
      "link" (flatten-one-level [:a {:src (:raw-link props)} contents])
      
      "paragraph" (flatten-one-level [:p contents])
      "bold" (flatten-one-level [:span.font-bold contents])
      "code" (flatten-one-level [:code (:value props)])
      "src-block" [:pre (flatten-one-level [:code (:value props)])]

      (print (str "not handled: " (:type element))))))

(pprint (w/postwalk
         (fn [form]
           (if (and (map? form)
                    (contains? form :props))
             (org-element->html form)
             form))
         org-data))

(defn main-panel []
  [:main.prose.mx-auto
   (w/postwalk
    (fn [form]
      (if (and (map? form)
               (contains? form :props))
        (org-element->html form)
        form))
    org-data)
   ])


[:div.section
 nil
 nil
 nil
 nil
 nil
 [:p
  "When "
  [:a
   {:src "id:4544AF53-2E56-4908-B1FB-018349BF9AA2"}
   "I made a blog with Emacs"]
  ",\n\n I decided to host my static website on AWS using "
  [:a
   {:src "id:62F7CC0C-E906-41C5-AA15-B8EA214BA5D6"}
   "CloudFormation"]
  ". To understand what I was doing,\n\n I broke down what I needed to do into smaller steps. All the code in this blog post is in "
  [:a
   {:src
    "https://github.com/genenakagaki/sample-cloudfront-static-site"}
   "my GitHub Repository"]
  ".\n"]
 :div
 :h1
 "The infrastructure of my website "
 [:span.font-bold "hello"]
 [:pre [:code "dc"]]
 [:div.section
  [:p
   [:span.font-bold "hello"]
   "\nI designed my infrastructure to be affordable. I didn't want to spend much money on a static site. If it would cost a lot,\n\n I would just use "
   [:a {:src "https://www.netlify.com/"} "Netlify"]
   "or something similar that I can host a static site for free. So why did I choose CloudFormation? I just wanted to learn "
   [:a
    {:src "id:2FBE1B2E-EC86-41DF-9EFC-DBFAD1927051"}
    "IaC (Infrastructure as Code)"]
   "and try it out on my website. Also,\n\n my tiny knowledge of IaC tells me that it going to make my website easier to maintain in the future.\n"]]
 [:div
  :h2
  "THE AWS services I "
  [:a {:src "http://testing"} "test"]
  "on "
  [:pre [:code "my"]]
  "website"
  [:div.section
   [:p
    "I kind of knew in my head,\n but I decided to make an AWS network diagram just to make things clear for me.\n"]
   nil
   [:p
    [:a {:src "file:../images/2022-07-30_09-30-30_screenshot.png"} nil]
    "\n"]
   [:p "I used the following AWS services:\n"]
   [:ul
    [:li
     [:p
      [:a {:src "id:11F8FAD0-5B25-4A53-8007-290D0C5E5F5C"} "Route 53"]
      "for "
      [:a {:src "id:131D05E9-52C2-4066-A086-5B664E2CA84E"} "DNS"]
      "\n"]]
    [:li
     [:p
      [:a
       {:src "id:D5572C29-14CD-4C34-8247-58AFF50E86BD"}
       "CloudFront"]
      "for "
      [:a {:src "id:5F5A615A-A1BD-4160-AB96-69AC0AE75519"} "CDN"]
      "\n"]]
    [:li
     [:p
      [:a {:src "id:89BB0856-B808-4B35-B936-739229936A43"} "S3"]
      "for storing website files\n"]]
    [:li
     [:p
      [:a
       {:src "id:2EFEA228-A0A7-4E28-B241-B3FFD75CA61F"}
       "AWS Certificate Manager"]
      "for "
      [:a
       {:src "id:815F583B-3371-49CE-B524-30D06B4405E4"}
       "SSL Certificate"]
      "(Not shown in the diagram)\n"]]]
   [:p
    "My project was very simple when shown just the "
    [:a
     {:src "id:CD1FC490-9B9E-4A6E-A0D8-642A2D99E7FF"}
     "AWS Services"]
    ",\n but I decided to add a little bit of description to make it easier to understand what's happening in the inside.\n"]
   nil
   [:p
    [:a {:src "file:../images/2022-07-30_09-31-02_screenshot.png"} nil]
    "\n"]
   [:p
    "This design works for me because all I have to pay is somewhere around $0.50 per month for Route 53. Even if more users come to my site,\n I won't have to worry that much because CloudFront can serve 1 TB of data transfer out and 10,\n000,\n000 HTTP or HTTPS Requests for free each month!\n"]]]
 :div
 :h1
 "Setting up the static website"
 [:div.section
  [:p
   "Now its time to create the infrastructure with CloudFormation! Starting from scratch was way too hard for me so I Googled for a template similar to my infrastructure design. I found "
   [:a
    {:src
     "https://github.com/aws-samples/amazon-cloudfront-secure-static-site"}
    "this repository on GitHub"]
   ". It was basically the same thing as my design. I probably could've used this as is,\n but it was using multiple CloudFormation stacks and it was very complex. To make this simpler for me,\n I took out all the stuff I didn't really need and made it into just one CloudFormation stack.\n"]
  [:p
   "In this part,\n I will create a website that serves HTML content in S3 through CloudFront. At the end of this part,\n I was able to access the static website through from the domain name provided by CloudFront.\n"]]
 [:div
  :h2
  "Setting up an S3 bucket"
  [:div.section
   [:p
    "First I needed a website root folder to serve the static site. This S3 configuration just creates an "
    [:a {:src "id:EB9D0F68-D101-4194-94AC-CD32050BB817"} "S3 Bucket"]
    "for the website root folder. \n"]
   [:pre
    [:code
     "  Resources:\n    WebsiteRootS3Bucket:\n      Type: AWS::S3::Bucket\n      Properties:\n        BucketName: !Sub '${AWS::StackName}-root-bucket'\n"]]
   [:p
    "The "
    [:pre [:code "BucketName"]]
    "property had to be globally unique,\n so I decided to prepend the bucket name with the "
    [:pre [:code "AWS::StackName"]]
    ". The "
    [:pre [:code "AWS::StackName"]]
    "is one of the "
    [:a
     {:src "id:65D2050F-87AE-4457-AFF4-F734CBE97CB2"}
     "CloudFormation Pseudo Parameter"]
    ",\n which are parameters predefined by AWS. I'll try to remember this,\n as this seems useful in a lot of situations.\n"]
   [:p
    "This is what it looks like when I create the CloudFormation stack on the "
    [:a
     {:src "id:25F4D469-990F-4551-B7ED-48535A2B1E65"}
     "AWS Management Console"]
    ".\n"]
   nil
   [:p
    [:a {:src "file:../images/2022-08-09_09-02-51_screenshot.png"} nil]
    "\n"]
   [:p
    "I specified it to be "
    [:pre [:code "gene-website"]]
    ",\n so "
    [:pre [:code "!Sub '${AWS::StackName}-root-bucket'"]]
    "is going to be evaluated as "
    [:pre [:code "gene-website-root-bucket"]]
    ".\n"]]]
 [:div
  :h2
  "Setting up a CloudFront Distribution"
  [:div.section
   [:p
    "Now that I have a S3 bucket,\n I need CloudFront to serve the static files. This part was harder because there were a lot of stuff going on. So I made a diagram of an overview of what "
    [:a
     {:src "id:20D8F1D9-F3F4-4480-BE9E-34A7C681E6B5"}
     "AWS Resource"]
    "are needed,\n and how they interact with each other. I didn't know making diagrams like this help me understand the infrastructure so much more. I should make this for every project.\n"]
   nil
   [:p
    [:a {:src "file:../images/2022-07-30_09-32-11_screenshot.png"} nil]
    "\n"]
   [:p
    "I broke it down into 3 parts to make it easier to understand.\n"]]
  [:div
   :h3
   "Part 1: Create the distribution!"
   [:div.section
    [:p
     "The center part of the CloudFront configuration is the "
     [:a
      {:src "id:0C686047-FE27-49A2-9AE1-C68C37F0B2F7"}
      "CloudFront Distribution"]
     "resource. A detailed explanation for each property is provided in the "
     [:a
      {:src
       "https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cloudfront-distribution-distributionconfig.html"}
      "AWS documentation"]
     ". \n"]
    [:pre
     [:code
      "  Resources:\n    ...\n\n    WebsiteCloudFrontDistribution:\n      Type: AWS::CloudFront::Distribution\n      Properties:\n        DistributionConfig:\n          Enabled: true\n          HttpVersion: 'http2'\n          IPV6Enabled: true\n          PriceClass: 'PriceClass_All'\n"]]]]
  [:div
   :h3
   "Part 2: Set up S3 as the origin"
   [:div.section
    [:p
     "Setting up the S3 bucket I created previously as the origin of the CloudFront requires a few resources to be set up correctly. To be clear of what this sections describes,\n I'll put an image of what I want to achieve.\n"]
    nil
    [:p
     [:a
      {:src "file:../images/2022-07-30_10-43-24_screenshot.png"}
      nil]
     "\n"]
    [:p
     "First you need the "
     [:a
      {:src "id:37EAF22A-EB6F-4DB5-B8BF-A7D6CF11818D"}
      "CloudFront Origin Access Identity"]
     "resource. This will be the user that CloudFront uses to access the S3 bucket. \n"]
    [:pre
     [:code
      "  Resources:\n    ...\n\n    WebsiteCloudFrontOAI:\n      Type: AWS::CloudFront::CloudFrontOriginAccessIdentity\n      Properties:\n        CloudFrontOriginAccessIdentityConfig:\n          Comment: 'CloudFront OAI for website'\n"]]
    [:p
     "Next you need a "
     [:a
      {:src "id:9D5AA985-6F08-40A8-8358-C4E02CAF9E53"}
      "S3 Bucket Policy"]
     "resource that allows the origin access identity to get stuff from the S3 bucket.  \n"]
    [:pre
     [:code
      "  Resources:\n    ...\n\n    WebsiteS3BucketPolicy:\n      Type: AWS::S3::BucketPolicy\n      Properties:\n        # The bucket created in the previous section\n        Bucket: !Ref WebsiteRootS3Bucket \n        PolicyDocument:\n          Version: '2012-10-17'\n          Statement:\n            - Action:\n                - s3:GetObject\n              Effect: Allow\n              Resource: !Join ['',\n [!GetAtt WebsiteRootS3Bucket.Arn,\n  '/*']]\n              Principal:\n                # The OriginAccessIdentity created in the previous section\n                CanonicalUser: !GetAtt WebsiteCloudFrontOAI.S3CanonicalUserId \n            - Action:\n                - s3:ListBucket\n              Effect: Allow\n              Resource: !GetAtt WebsiteRootS3Bucket.Arn\n              Principal:\n                # The OriginAccessIdentity created in the previous section\n                CanonicalUser: !GetAtt WebsiteCloudFrontOAI.S3CanonicalUserId\n"]]
    [:p
     "Now that the origin access identity can access the S3 resources,\n I needed to set up the S3 origin in the distribution. I set it up so that the distribution uses the origin access identity to access the S3 bucket.\n"]
    [:pre
     [:code
      "  Resources:\n    ...\n\n    WebsiteCloudFrontDistribution:\n      Type: AWS::CloudFront::Distribution\n      Properties:\n        DistributionConfig:\n          ...\n          Origins:\n            - Id: 'website-root-s3'\n              # The bucket created in the previous section\n              DomainName: !GetAtt WebsiteRootS3Bucket.DomainName\n              S3OriginConfig:\n                OriginAccessIdentity:\n                  # The OriginAccessIdentity created in the previous section\n                  !Join ['',\n ['origin-access-identity/cloudfront/',\n !Ref WebsiteCloudFrontOAI]]\n"]]
    [:p
     "The CloudFront distribution can now get stuff from the S3 bucket! \n"]]]
  [:div
   :h3
   "Part 3: Set up the default behavior"
   [:div.section
    [:p
     "Now that the communications with the S3 bucket is established,\n a default behavior is required to be set up for CloudFront to start working. The "
     [:a
      {:src "id:7F4C3A91-949F-4C4D-84E2-6985862973ED"}
      "CloudFront Cache Policy"]
     "describes how stuff are cached.\n"]
    [:pre
     [:code
      "  Resources:\n    ...\n  \n    WebsiteCloudFrontCachePolicy:\n      Type: AWS::CloudFront::CachePolicy\n      Properties: \n        CachePolicyConfig:\n          Name: Sub '${AWS::StackName}-cache-policy'\n          Comment: 'CachePolicy for website'\n          DefaultTTL: 86400 # in seconds (one day)\n          MaxTTL: 31536000 # in seconds (one year)\n          MinTTL: 1 # Must be at least 1\n          ParametersInCacheKeyAndForwardedToOrigin: \n            CookiesConfig:\n              CookieBehavior: 'none'\n            EnableAcceptEncodingBrotli: true\n            EnableAcceptEncodingGzip: true\n            HeadersConfig: \n              HeaderBehavior: 'none'\n            QueryStringsConfig:\n              QueryStringBehavior: 'none'\n"]]
    [:p
     "The "
     [:pre [:code "Name"]]
     "property had to be globally unique,\n so I decided to prefix it with the stack name by using the pseudo parameter again. The setting is the same as one of the managed cache policy,\n which is explained in "
     [:a
      {:src
       "https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/using-managed-cache-policies.html"}
      "the docs"]
     ". It is the same as the "
     [:pre [:code "CachingOptimized"]]
     "managed cache policy. I didn't really need a specialized caching behavior so I used the same configuration as this one. I could have just used the "
     [:pre [:code "CachingOptimized"]]
     "managed cache policy ID (it's in the docs) and skip creating  this cache policy,\n but I wanted to be able to see how stuff are cached in the code. \n"]
    [:p
     "I set up the default behavior with my new cache policy like this.\n"]
    [:pre
     [:code
      "  Resources:\n    ...\n\n    WebsiteCloudFrontDistribution:\n      Type: AWS::CloudFront::Distribution\n      Properties:\n        DistributionConfig:\n          ...\n          DefaultCacheBehavior:\n            # The cache policy created in the previous section\n            CachePolicyId: !Ref WebsiteCloudFrontCachePolicy\n            Compress: true\n            # The id of the origin created in the previous section\n            TargetOriginId: 'website-root-s3'\n            ViewerProtocolPolicy: 'redirect-to-https'\n          DefaultRootObject: 'index.html'\n          CustomErrorResponses:\n            - ErrorCachingMinTTL: 60\n              ErrorCode: 404\n              ResponseCode: 404\n              ResponsePagePath: '/404.html'\n"]]
    [:p
     "This configuration makes the CloudFront distribution to look for files in the S3 bucket by default. It also define which page to show at the root path,\n and which page to show when the user gets a 404 response.\n"]
    [:p
     "I can now access my static site! I was able to access the site by accessing the "
     [:pre [:code "Distribution domain name"]]
     ". It was in the "
     [:a
      {:src "id:25F4D469-990F-4551-B7ED-48535A2B1E65"}
      "AWS Management Console"]
     "in the CloudFront service page.\n"]]]]
 :div
 :h1
 "Making CloudFront accessible from my domain with CloudFormation"
 [:div.section
  [:p
   "Now that CloudFront is set up,\n I want to access it from "
   [:pre [:code "genenakagaki.com"]]
   ". In order to do this I need to configure "
   [:a
    {:src "id:11F8FAD0-5B25-4A53-8007-290D0C5E5F5C"}
    "Route 53 (AWS)"]
   "and "
   [:a
    {:src "id:2EFEA228-A0A7-4E28-B241-B3FFD75CA61F"}
    "AWS Certificate Manager"]
   ". At the end of this section I was able to access the CloudFront from "
   [:pre [:code "genenakagaki.com"]]
   ".\n"]]
 [:div
  :h2
  "Setting up Route 53"
  [:div.section
   [:p
    "In order to access CloudFront from my domain,\n I needed to make Route 53 point to the CloudFront distribution for the "
    [:pre [:code "genenakagaki.com"]]
    "domain.\n"]
   [:pre
    [:code
     "  Resources:\n    ...\n\n    WebsiteRoute53RecordSetGroup:\n      Type: AWS::Route53::RecordSetGroup\n      Properties:\n        HostedZoneName: 'genenakagaki.com.'\n        RecordSets:\n        - Name: 'genenakagaki.com'\n          # This is for IPv4\n          Type: 'A'\n          AliasTarget:\n            DNSName: !GetAtt WebsiteCloudFrontDistribution.DomainName\n            EvaluateTargetHealth: false\n            # The  following HosteZoneId is always used for alias records pointing to CF.\n            HostedZoneId: 'Z2FDTNDATAQYW2'\n        - Name: 'genenakagaki.com'\n          # Required for IPv6\n          Type: 'AAAA'\n          AliasTarget:\n            DNSName: !GetAtt WebsiteCloudFrontDistribution.DomainName\n            EvaluateTargetHealth: false\n            # The  following HosteZoneId is always used for alias records pointing to CF.\n            HostedZoneId: 'Z2FDTNDATAQYW2'\n\n"]]
   [:p
    "One thing to be careful is the "
    [:pre [:code "HostedZoneName"]]
    ". The dot ("
    [:pre [:code "."]]
    ") at the end of the domain name is required. If it's not there,\n CloudFormation is going to complain that it can't find the "
    [:a {:src "id:FBA01C88-7035-402E-B742-E749F4750AEE"} "Hosted Zone"]
    ". For the "
    [:a
     {:src "id:47688D14-0FB8-4B90-A193-D83083CFB1F9"}
     "Route 53 Record Set"]
    ",\n a detailed guide of what you need to configure is in the "
    [:a
     {:src
      "https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/resource-record-sets-values-alias.html"}
     "AWS documentation"]
    ". This configuration points "
    [:a {:src "id:4F19CCE8-BBE4-4B4B-8E82-B71F7E4A7FD5"} "IPv4"]
    "and "
    [:a {:src "id:98BF976D-BC59-4FFF-9E25-EF3E176043BE"} "IPv6"]
    "requests to "
    [:pre [:code "genenakagaki.com"]]
    "to the CloudFront distribution.\n"]
   [:p
    "Now that Route 53 is configured,\n I needed to configure CloudFront to be able to handle HTTPS requests.\n"]]]
 [:div
  :h2
  "Setting up HTTPS requests to CloudFront"
  [:div.section
   [:p
    "First I needed an "
    [:a
     {:src "id:815F583B-3371-49CE-B524-30D06B4405E4"}
     "SSL Certificate"]
    ".\n"]
   [:pre
    [:code
     "  Parameters:\n    HostedZoneId:\n      Description: HostedZoneId for the domain e.g. Z23ABC4XYZL05B\n      Type: String\n\n  Resources:\n    WebsiteCertificate:\n      Type: AWS::CertificateManager::Certificate\n      Properties:\n        DomainName: 'genenakagaki.com'\n        DomainValidationOptions:\n          - DomainName: 'genenakagaki.com'\n            HostedZoneId: !Ref HostedZoneId\n        ValidationMethod: DNS\n\n"]]
   [:p
    "I needed to specify a "
    [:a {:src "id:FBA01C88-7035-402E-B742-E749F4750AEE"} "Hosted Zone"]
    "to verify the SSL certificate,\n but I decided not to make the Hosted Zone in this CloudFormation template. Since I am planning on making multiple projects in the same Hosted Zone,\n having the Hosted Zone living in this particular project would be confusing to the future me.\n"]
   [:p
    "The "
    [:a {:src "id:12445406-CEC1-4F7F-B548-9CCB7F6E03C9"} "Parameter"]
    "adds a new input box when you create the CloudFormation stack on the "
    [:a
     {:src "id:25F4D469-990F-4551-B7ED-48535A2B1E65"}
     "AWS Management Console"]
    ". Mine looked something like this.\n"]
   nil
   [:p
    [:a {:src "file:../images/2022-08-09_08-53-08_screenshot.png"} nil]
    "\n"]
   [:p
    "Now that I have an SSL certificate,\n I needed to tell CloudFront to use that certificate.\n"]
   [:pre
    [:code
     "  Resources:\n    ...\n\n    WebsiteCloudFrontDistribution:\n      Type: AWS::CloudFront::Distribution\n      Properties:\n        DistributionConfig:\n          Aliases:\n            - 'genenakagaki.com'\n          ...\n          ViewerCertificate:\n            # The Certificate created in the previous section\n            AcmCertificateArn: !Ref WebsiteCertificate\n            MinimumProtocolVersion: 'TLSv1.1_2016'\n            SslSupportMethod: 'sni-only'\n"]]
   [:p
    "After setting the "
    [:pre [:code "Aliases"]]
    "and "
    [:pre [:code "ViewerCertificate"]]
    ",\n I was able to create the CloudFormation stack and access CloudFront from "
    [:a {:src "https://genenakagaki.com"} nil] 
    "!\n"]]]]
