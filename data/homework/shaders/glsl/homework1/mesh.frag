#version 450

//layout (set = 1, binding = 0) uniform sampler2D samplerColorMap;
layout(set = 1, binding = 0) uniform sampler2D samplerColorMap0;
layout(set = 1, binding = 1) uniform sampler2D samplerMetallicRoughness_OcclusionMap;
layout(set = 1, binding = 2) uniform sampler2D samplerNormalMap;
layout(set = 1, binding = 3) uniform sampler2D samplerBRDFLutMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inWorldPos;
layout (location = 4) in vec3 inTangent;
layout (location = 5) in vec3 inBitangent;

layout (location = 0) out vec4 outFragColor;

layout (set = 0, binding = 0) uniform UBOScene
{
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

#define PI 3.1415926535897932384626433832795
#define ALBEDO pow(texture(samplerColorMap0, inUV).rgb, vec3(2))

// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, vec3 F0)
{
	return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}
vec3 F_SchlickR(float cosTheta, vec3 F0, float roughness)
{
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

vec3 calNormal()
{
	vec3 tangentNormal = texture(samplerNormalMap, inUV).xyz * 2.0 - 1.0;

	vec3 N = normalize(inNormal);
	vec3 T = normalize(inTangent);
	vec3 B = normalize(inBitangent);
	mat3 TBN = mat3(T,B,N);
	return normalize(TBN * tangentNormal);
}

void main() 
{
	vec3 Color = vec3(0);
   
   vec3 BaseColor = texture(samplerColorMap0, inUV).rgb * inColor.xyz;
	vec3 N = calNormal();
	vec3 L = normalize(uboScene.lightPos.xyz - inWorldPos);
	vec3 V = normalize(uboScene.viewPos.xyz - inWorldPos);
	vec3 R = reflect(-V, N);

	float Metallic = texture(samplerMetallicRoughness_OcclusionMap, inUV).b;
	float Roughness = texture(samplerMetallicRoughness_OcclusionMap, inUV).g;

	vec3 F0 = vec3(0.04);
	F0 = mix(F0, BaseColor, Metallic);

	//light
	vec3 H = normalize(L + V);
	float NDotH = clamp(dot(N, H), 0.0, 1.0);
	float NDotV = clamp(dot(N, V), 0.0, 1.0);
	float NDotL = clamp(dot(N, L), 0.0, 1.0);

	if (NDotL > 0.0) 
	{
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(NDotH, Roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(NDotL, NDotV, Roughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
		vec3 F = F_Schlick(NDotV, F0);		
		vec3 spec = D * F * G / (4.0 * NDotL * NDotV + 0.001);		
		vec3 kD = (vec3(1.0) - F) * (1.0 - Metallic);			
		Color += (kD * BaseColor / PI + spec) * NDotL;
	}

	float Occlusion = texture(samplerMetallicRoughness_OcclusionMap, inUV).r;

	vec3 reflection = vec3(1);
	vec2 brdf = texture(samplerBRDFLutMap, vec2(NDotV, Roughness)).rg;

	vec3 F = F_SchlickR(max(dot(N, V), 0.0), F0, Roughness);
	vec3 kD = 1.0 - F;
	kD *= 1.0 - Metallic;

	vec3 diffuse = BaseColor / PI;
	vec3 specular = reflection * (F * brdf.x + brdf.y);

	Color += (kD * diffuse + specular) * Occlusion;
	
	vec3 DebugColor = vec3(1);

	outFragColor = vec4(Color, 1.0);	
}