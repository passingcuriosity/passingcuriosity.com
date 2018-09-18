resource "aws_s3_bucket" "hosting" {
  bucket        = "passingcuriosity.com"
  acl           = "private"
  force_destroy = false

  website {
    index_document = "index.html"
    error_document = "4xx.html"
  }

  tags {
    Site = "passingcuriosity.com"
  }
}

resource "aws_acm_certificate" "cert" {
  provider = "aws.global"

  domain_name               = "passingcuriosity.com"
  subject_alternative_names = ["www.passingcuriosity.com"]
  validation_method         = "DNS"

  lifecycle {
    create_before_destroy = true
  }

  tags {
    Site = "passingcuriosity.com"
  }
}

resource "aws_acm_certificate_validation" "cert" {
  provider        = "aws.global"
  certificate_arn = "${aws_acm_certificate.cert.arn}"

  validation_record_fqdns = [
    "${aws_route53_record.cert.fqdn}",
    "${aws_route53_record.cert-www.fqdn}",
  ]
}

resource "aws_iam_role" "redirect" {
  name = "passingcuriosity-redirect"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": [
            "edgelambda.amazonaws.com",
            "lambda.amazonaws.com"
        ]
      },
      "Effect": "Allow"
    }
  ]
}
EOF
}

resource "aws_iam_policy" "redirect" {
  name        = "passingcuriosity-redirect"
  description = "Policy for web redirects on passingcuriosity.com"

  policy = <<EOF
{
    "Version": "2012-10-17",
    "Statement":[
        {
        "Effect": "Allow",
        "Action": [
            "logs:CreateLogGroup",
            "logs:CreateLogStream",
            "logs:PutLogEvents"
        ],
        "Resource": [
            "arn:aws:logs:*:*:*"
        ]
        }
    ]
}
EOF
}

resource "aws_iam_role_policy_attachment" "redirect" {
  role       = "${aws_iam_role.redirect.name}"
  policy_arn = "${aws_iam_policy.redirect.arn}"
}

data "archive_file" "redirect_code" {
  type        = "zip"
  output_path = "${path.module}/files/redirect.zip"

  source {
    content  = "${file("${path.module}/files/redirect.js")}"
    filename = "index.js"
  }
}

resource "aws_lambda_function" "redirect" {
  provider = "aws.global"

  function_name = "passingcuriosity-redirect"
  description   = "Sensible redirects."
  role          = "${aws_iam_role.redirect.arn}"
  runtime       = "nodejs6.10"
  memory_size   = "128"
  timeout       = "3"
  handler       = "index.handler"
  publish       = true

  filename         = "${substr(data.archive_file.redirect_code.output_path, length(path.cwd) + 1, -1)}"
  source_code_hash = "${data.archive_file.redirect_code.output_base64sha256}"

  tags {
    Site = "passingcuriosity.com"
  }
}

resource "aws_cloudfront_distribution" "hosting" {
  enabled             = true
  is_ipv6_enabled     = true
  default_root_object = "index.html"

  aliases = ["passingcuriosity.com", "www.passingcuriosity.com"]

  tags {
    Site = "passingcuriosity.com"
  }

  origin {
    origin_id   = "S3-passingcuriosity.com"
    domain_name = "passingcuriosity.com.s3.amazonaws.com"
  }

  default_cache_behavior {
    target_origin_id       = "S3-passingcuriosity.com"
    viewer_protocol_policy = "redirect-to-https"
    allowed_methods        = ["HEAD", "GET"]
    cached_methods         = ["HEAD", "GET"]

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }

    lambda_function_association {
      event_type   = "origin-request"
      include_body = false

      lambda_arn = "${aws_lambda_function.redirect.qualified_arn}"

      #lambda_arn = "arn:aws:lambda:us-east-1:806816311130:function:aws-serverless-repository-StandardRedirectsForClou-QAF83TUFT3WH:1"
    }
  }

  viewer_certificate {
    acm_certificate_arn      = "${aws_acm_certificate.cert.arn}"
    minimum_protocol_version = "TLSv1.1_2016"
    ssl_support_method       = "sni-only"
  }

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }
}

resource "aws_route53_zone" "hosting" {
  name          = "passingcuriosity.com"
  comment       = ""
  force_destroy = false

  tags {
    Site = "passingcuriosity.com"
  }

}

resource "aws_route53_record" "root" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  name            = "passingcuriosity.com"
  type            = "A"
  allow_overwrite = true

  alias {
    name                   = "${aws_cloudfront_distribution.hosting.domain_name}"
    zone_id                = "${aws_cloudfront_distribution.hosting.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "www" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  name            = "www.passingcuriosity.com"
  type            = "A"
  allow_overwrite = true

  alias {
    name                   = "${aws_cloudfront_distribution.hosting.domain_name}"
    zone_id                = "${aws_cloudfront_distribution.hosting.hosted_zone_id}"
    evaluate_target_health = false
  }
}

resource "aws_route53_record" "mx" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  name            = "passingcuriosity.com"
  type            = "MX"
  ttl             = "300"
  records         = ["20 in2-smtp.messagingengine.com", "10 in1-smtp.messagingengine.com"]
  allow_overwrite = true
}

resource "aws_route53_record" "xmpp-client" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  name            = "_xmpp-client._tcp.passingcuriosity.com"
  type            = "SRV"
  ttl             = "300"
  records         = ["5 0 5222 chat.messagingengine.com"]
  allow_overwrite = true
}

resource "aws_route53_record" "xmpp-server" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  name            = "_xmpp-server._tcp.passingcuriosity.com"
  type            = "SRV"
  ttl             = "300"
  records         = ["5 0 5269 chat.messagingengine.com"]
  allow_overwrite = true
}

resource "aws_route53_record" "cert" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  ttl             = 60
  allow_overwrite = true

  name    = "${aws_acm_certificate.cert.domain_validation_options.0.resource_record_name}"
  type    = "${aws_acm_certificate.cert.domain_validation_options.0.resource_record_type}"
  records = ["${aws_acm_certificate.cert.domain_validation_options.0.resource_record_value}"]
}

resource "aws_route53_record" "cert-www" {
  zone_id         = "${aws_route53_zone.hosting.zone_id}"
  ttl             = 60
  allow_overwrite = true

  name    = "${aws_acm_certificate.cert.domain_validation_options.1.resource_record_name}"
  type    = "${aws_acm_certificate.cert.domain_validation_options.1.resource_record_type}"
  records = ["${aws_acm_certificate.cert.domain_validation_options.1.resource_record_value}"]
}
